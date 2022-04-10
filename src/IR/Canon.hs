{- Based on code by Andrew Appel.
   Modified by Chris Stone for CS 132
-}
module IR.Canon where

import qualified Data.Map                      as Map
import qualified Data.IORef
import           IR.Temp
import qualified IR.Target                        as T
import qualified IR.NormTarget                    as NT

-----------------------
-- Fresh Temporaries --
-----------------------

freshTemp :: Counter -> IO Temp
freshTemp = mkFreshTemp "z"

freshLabel :: Counter -> IO T.Label
freshLabel ct = do
  next <- Data.IORef.readIORef ct
  Data.IORef.writeIORef ct (next + 1)
  return ("Lz" ++ show next)

-----------------------
-- Fragment Building --
----------------------

extractFrags :: Counter -> T.Stmt -> IO ([T.Fragment], T.Stmt)
extractFrags ct stm0 = doStm stm0
 where
  doStm (T.SEQ stms) = do
    (frags, stms') <- doStms stms
    return (frags, T.SEQ stms')
  doStm (T.CJUMP rop exp1 exp2 label1 label2) = do
    (frags1, exp1') <- doExp exp1
    (frags2, exp2') <- doExp exp2
    return (frags1 ++ frags2, T.CJUMP rop exp1' exp2' label1 label2)
  doStm (T.ASSIGN exp1 exp2) = do
    (frags1, exp1') <- doExp exp1
    (frags2, exp2') <- doExp exp2
    return (frags1 ++ frags2, T.ASSIGN exp1' exp2')
  doStm (T.EXPR expr) = do
    (frags, exp') <- doExp expr
    return (frags, T.EXPR exp')
  doStm (stm@(T.LABEL  _      )) = return ([], stm)
  doStm (stm@(T.JUMP   _      )) = return ([], stm)
  doStm (stm@(T.RETURN Nothing)) = return ([], stm)
  doStm (T.RETURN (Just expr)  ) = do
    (frags, exp') <- doExp expr
    return (frags, T.RETURN (Just exp'))


  doStms []           = return ([], [])
  doStms (stm : stms) = do
    (frags1, stm' ) <- doStm stm
    (frags2, stms') <- doStms stms
    return (frags1 ++ frags2, stm' : stms')

  doExp (T.INTS bytes ns) = do
    l <- freshLabel ct
    return ([T.FragInts l bytes ns], T.NAME l)
  doExp (T.BINOP exp1 bop exp2) = do
    (frags1, exp1') <- doExp exp1
    (frags2, exp2') <- doExp exp2
    return (frags1 ++ frags2, T.BINOP exp1' bop exp2')
  doExp (T.MEM bytes expr) = do
    (frags, exp') <- doExp expr
    return (frags, T.MEM bytes exp')
  doExp (T.ESEQ stms expr) = do
    (frags1, stms') <- doStms stms
    (frags2, exp' ) <- doExp expr
    return (frags1 ++ frags2, T.ESEQ stms' exp')
  doExp (T.CALL expr exps) = do
    (frags1, exp' ) <- doExp expr
    (frags2, exps') <- doExps exps
    return (frags1 ++ frags2, T.CALL exp' exps')
  doExp (expr@(T.TEMP  _)) = return ([], expr)
  doExp (expr@(T.NAME  _)) = return ([], expr)
  doExp (expr@(T.CONST _)) = return ([], expr)

  doExps []            = return ([], [])
  doExps (expr : exps) = do
    (frags1, exp' ) <- doExp expr
    (frags2, exps') <- doExps exps
    return (frags1 ++ frags2, exp' : exps')


-------------------
-- Linearization --
-------------------

linearize :: Counter -> T.Stmt -> IO [T.Stmt]
 -- From an arbitrary Target statement, produce a list of cleaned
 --   statements satisfying the following properties:
 --      1.  No SEQ's or ESEQ's
 --      2.  The parent of every CALL is an EXP(..) or a ASSIGN(TEMP t,..)
linearize ct stm0 =
  let
    commute ([T.EXPR (T.CONST _)], _) = True
    commute (_, T.NAME _) = True
    commute (_, T.CONST _) = True
    commute ([], _) = True {- ??? -}
    commute _ = False

    -- nop = T.EXPR (T.CONST 0)

    reorder :: [T.Expr] -> IO ([T.Stmt], [T.Expr])
    reorder ((e@(T.CALL _ _)) : rest) = do
      t <- freshTemp ct
      reorder (T.ESEQ [T.ASSIGN (T.TEMP t) e] (T.TEMP t) : rest)

    reorder (expr : rest) = do
      (stms , e ) <- do_exp expr
      (stms', el) <- reorder rest
      if commute (stms', e)
        then return (stms ++ stms', e : el)
        else do
          t <- freshTemp ct
          return (stms ++ [T.ASSIGN (T.TEMP t) e] ++ stms', T.TEMP t : el)

    reorder [] = return ([], [])

    reorder_exp :: [T.Expr] -> ([T.Expr] -> T.Expr) -> IO ([T.Stmt], T.Expr)
    reorder_exp exps build = do
      (stms, exps') <- reorder exps
      return (stms, build exps')

    reorder_stm :: [T.Expr] -> ([T.Expr] -> T.Stmt) -> IO [T.Stmt]
    reorder_stm exps build = do
      (stms, exps') <- reorder exps
      return (stms ++ [build exps'])

    do_stm (T.SEQ stms) = do
      stmLists <- mapM do_stm stms
      return (concat stmLists)
    do_stm (T.JUMP l) = reorder_stm [T.NAME l] (\[T.NAME l'] -> T.JUMP l')
    do_stm (T.CJUMP p a b t f) =
      reorder_stm [a, b] (\[a', b'] -> T.CJUMP p a' b' t f)
    do_stm (T.ASSIGN (T.TEMP t) (T.CALL e el)) = reorder_stm
      (e : el)
      (\es -> T.ASSIGN (T.TEMP t) (T.CALL (head es) (tail es)))
    do_stm (T.ASSIGN (T.TEMP t) b) =
      reorder_stm [b] (\[b'] -> T.ASSIGN (T.TEMP t) b')
    do_stm (T.ASSIGN (T.MEM n e) b) =
      reorder_stm [e, b] (\[e', b'] -> T.ASSIGN (T.MEM n e') b')
    do_stm (T.ASSIGN (T.ESEQ s e) b) = do_stm (T.SEQ (s ++ [T.ASSIGN e b]))
    do_stm (T.EXPR (T.CALL e el)) =
      reorder_stm (e : el) (\es -> T.EXPR (T.CALL (head es) (tail es)))
    do_stm (T.EXPR   e       ) = reorder_stm [e] (\[e'] -> T.EXPR e')
    do_stm (T.RETURN (Just e)) = reorder_stm [e] (\[e'] -> T.RETURN (Just e'))
    do_stm s                   = reorder_stm [] (\_ -> s)

    do_exp (T.BINOP a p b) = reorder_exp [a, b] (\[a', b'] -> T.BINOP a' p b')
    do_exp (T.MEM  n  a  ) = reorder_exp [a] (\[a'] -> T.MEM n a')
    do_exp (T.ESEQ ss e  ) = do
      stms        <- do_stm (T.SEQ ss)
      (stms', e2) <- reorder_exp [e] (\[e'] -> e')
      return (stms ++ stms', e2)
    do_exp (T.CALL e el) =
      reorder_exp (e : el) (\es -> T.CALL (head es) (tail es))
    do_exp e = reorder_exp [] (\_ -> e)
  in {- body of linearize -}
    do_stm stm0

type Block = [T.Stmt]

basicBlocks :: Counter -> [T.Stmt] -> IO ([Block], T.Label)
  {- From a list of cleaned trees, produce a list of
         basic blocks satisfying the following properties:
              1. and 2. as above;
              3.  Every block begins with a LABEL;
              4.  A LABEL appears only at the beginning of a block;
              5.  Any JUMP or CJUMP or RETURN is the last stm in a block;
              6.  Every block ends with a JUMP or CJUMP or RETURN;
           Also produce the "label" to which control will be passed
           upon exit.
  -}

basicBlocks ct stms = do
  done <- freshLabel ct

  let
    blocks :: ([T.Stmt], [Block]) -> IO [Block]
    blocks ((head'@(T.LABEL _)) : tail', blist) =
      let
        next ((s@(T.JUMP _)) : rest, thisblock) =
          endblock (rest, s : thisblock)
        next ((s@(T.CJUMP _ _ _ _ _)) : rest, thisblock) =
          endblock (rest, s : thisblock)
        next ((s@(T.RETURN _)) : rest, thisblock) =
          endblock (rest, s : thisblock)
        next (stms'@(T.LABEL lab : _), thisblock) =
          next (T.JUMP lab : stms', thisblock)
        next (s : rest, thisblock) = next (rest, s : thisblock)
        next ([]      , thisblock) = next ([T.RETURN Nothing], thisblock)

        endblock (stms', thisblock) = blocks (stms', reverse thisblock : blist)
      in
        next (tail', [head'])
    blocks ([]   , blist) = return (reverse blist)
    blocks (stms', blist) = do
      newLabelForNextBlock <- freshLabel ct
      blocks (T.LABEL newLabelForNextBlock : stms', blist)

  finalBlocks <- blocks (stms, [])
  return (finalBlocks, done)


type Table = Map.Map T.Label Block

traceSchedule :: Counter -> ([Block], T.Label) -> IO [T.Stmt]
 {- From a list of basic blocks satisfying properties 1-6,
            along with an "exit" label,
            produce a list of stms such that:
              1. and 2. as above;
              7. Every CJUMP(_,t,f) is immediately followed by LABEL f.
            The blocks are reordered to satisfy property 7; also
            in this reordering as many JUMP(T.NAME(lab)) statements
            as possible are eliminated by falling through into T.LABEL(lab).
  -}
traceSchedule ct (blocks, _) = do
  stms <- getnext (foldr enterblock Map.empty blocks, blocks)
  return stms

 where
  enterblock :: Block -> Table -> Table
  enterblock (b@(T.LABEL s : _)) table = Map.insert s b table
  enterblock _                   table = table

  splitlast lst = (init lst, last lst)

  trace (table0, b@(T.LABEL lab : _), rest) =
    let table = Map.insert lab [] table0
    in
      case splitlast b of
        (most, T.JUMP lab') ->
          (case Map.lookup lab' table of
            Just (b'@(_ : _)) -> do
              more <- trace (table, b', rest)
              return (most ++ more)
            _ -> do
              more <- getnext (table, rest)
              return (b ++ more)
          )

        (most, T.CJUMP opr x y t f) ->

          (case (Map.lookup t table, Map.lookup f table) of
            (_, Just (b'@(_ : _))) -> do
              more <- trace (table, b', rest)
              return (b ++ more)

            (Just (b'@(_ : _)), _) -> do
              more <- trace (table, b', rest)
              return (most ++ [T.CJUMP (T.notRel opr) x y f t] ++ more)

            (_, _) -> do
              f'   <- freshLabel ct
              more <- getnext (table, rest)
              return
                (most ++ [T.CJUMP opr x y t f', T.LABEL f', T.JUMP f] ++ more)
          )

        -- (most, T.JUMP _) -> do
        --   more <- getnext (table, rest)
        --   return (b ++ more)

        (_, T.RETURN _) -> do
          more <- getnext (table, rest)
          return (b ++ more)

        (_, s) ->
          error
            $  "Canon.traceSchedule: bad last instruction of a block: "
            ++ show s

  trace _ = error $ "Canon.trace: basic block doesn't start with a label"

  getnext (table, (b@(T.LABEL lab : _)) : rest) =
    (case Map.lookup lab table of
      Just (_ : _) -> trace (table, b, rest)
      _            -> getnext (table, rest)
    )
  getnext (_, []) = return []
  getnext _ = error $ "Canon.getNext: basic block doesn't start with a label"

xStmt :: T.Stmt -> [NT.Stmt]
xStmt (T.SEQ   _              ) = error "Canon.xStmt found a SEQ"
xStmt (T.LABEL l              ) = [NT.LABEL l]
xStmt (T.JUMP  l              ) = [NT.JUMP l]
xStmt (T.CJUMP rop e1 e2 lt lf) = [NT.CJUMP rop (xExpr e1) (xExpr e2) lt lf]
xStmt (T.ASSIGN (T.TEMP t) (T.CALL e es)) =
  [NT.CALL (Just t) (xExpr e) (map xExpr es)]
xStmt (T.EXPR (T.CALL e es)) = [NT.CALL Nothing (xExpr e) (map xExpr es)]
xStmt (T.EXPR _            ) = []
xStmt (T.ASSIGN e1 e2      ) = [NT.ASSIGN (xExpr e1) (xExpr e2)]
xStmt (T.RETURN Nothing    ) = [NT.RETURN Nothing]
xStmt (T.RETURN (Just e)   ) = [NT.RETURN (Just (xExpr e))]

xExpr :: T.Expr -> NT.Expr
xExpr (T.BINOP e1 bop e2) = NT.BINOP (xExpr e1) bop (xExpr e2)
xExpr (T.MEM bytes e    ) = NT.MEM bytes (xExpr e)
xExpr (T.TEMP t         ) = NT.TEMP t
xExpr (T.ESEQ _ _       ) = error "Canon.xExpr found an ESEQ"
xExpr (T.NAME  l        ) = NT.NAME l
xExpr (T.CONST i        ) = NT.CONST i
xExpr (T.CALL _ _       ) = error "Canon.xExpr found a CALL"
xExpr (T.INTS _ _       ) = error "Canon.xExpr found an INTS"

xFragments :: Counter -> [T.Fragment] -> IO [NT.Fragment]
xFragments _  []                             = return []
xFragments ct (T.FragInts l bytes ns : rest) = do
  let firstFrag = NT.FragInts l bytes ns
  restFrags <- xFragments ct rest
  return $ firstFrag : restFrags
xFragments ct (T.FragLabels l ls : rest) = do
  let firstFrag = NT.FragLabels l ls
  restFrags <- xFragments ct rest
  return $ firstFrag : restFrags
xFragments ct (T.FragCode l s : rest) = do
  (moreFrags, stms) <- canonicalize ct (T.SEQ s)
  let firstFrag = NT.FragCode l stms
  restFrags <- xFragments ct rest
  return $ firstFrag : moreFrags ++ restFrags

canonicalize :: Counter -> T.Stmt -> IO ([NT.Fragment], [NT.Stmt])
canonicalize ct stmt = do
  (frags, stmt')  <- extractFrags ct stmt
  linearStmts     <- linearize ct stmt'
  -- putStrLn "LinearStmts: "
  -- print linearStmts
  -- putStrLn ""
  (bblocks, done) <- basicBlocks ct linearStmts
  -- putStrLn "bblocks: "
  -- print bblocks
  -- putStrLn "done: "
  -- print done
  tracedStmts     <- traceSchedule ct (bblocks, done)
  canonFrags      <- xFragments ct frags
  let canonStms = concatMap xStmt tracedStmts
  return (canonFrags, canonStms)
