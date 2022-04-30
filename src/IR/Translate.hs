--{-# OPTIONS_GHC -fno-warn-unused-matches #-}

 {-|
Module       : Translate
Description  : "Lower" the code from Tasty to Target
Maintainer   : CS 132
-}

module IR.Translate where

import qualified Backend.X86gen as X86gen
import           Data.Char      (ord)
import qualified Data.IORef
import           IR.Target      as Target
import           IR.Tasty       as Tasty
import           IR.Temp        as Temp
import           Data.Maybe ( fromMaybe )

---------------------------------------------------
-- Generate new Temporaries and Labels on Demand --
---------------------------------------------------
freshTemp :: Counter -> IO Temp
freshTemp = Temp.mkFreshTemp "l"

freshLabel :: Counter -> IO Label
freshLabel ct = do
  next <- Data.IORef.readIORef ct
  Data.IORef.writeIORef ct (next + 1)
  return ("Ll" ++ show next)

-----------------------------------------------------------------------------------------
-- Programs
-----------------------------------------------------------------------------------------
xProg :: Counter -> Program -> IO [Fragment]
xProg ct decls = do frags <- mapM (xDecl ct) decls
                    return $ concat frags

-----------------------------------------------------------------------------------------
-- Declarations
-----------------------------------------------------------------------------------------
xDecl :: Counter -> Declaration -> IO [Fragment]

{- Function declarations -}
xDecl ct (DeclFunc fnName params body) = do
  body' <- xBody ct params body
  return [FragCode fnName body']

{- Class declarations -}
xDecl ct (DeclClassCode className constructor methods vtable) = do
  constructorFrag <- xConstructor ct className constructor
  vtableFrag      <- xVTable ct className vtable
  methodFrags     <- mapM (xMethod ct className) methods
  return $ constructorFrag : vtableFrag : methodFrags

{- Helper functions for declarations -}

-- function / method bodies
xBody :: Counter -> [Parameter] -> Tasty.Stmt -> IO [Target.Stmt]
xBody ct params body = do
  let moveParams = [  ASSIGN (TEMP p) (TEMP r)
                    | (r, p) <- zip X86gen.argumentRegisters params ]
  bodyCode <- xStmt ct body
  return $ moveParams ++ bodyCode

-- constructors
xConstructor :: Counter -> ClassName -> Constructor -> IO Fragment
xConstructor ct className (params, superInits, body) = do
  body' <- xBody ct params body
  -- let self = head params
  superCallingCode <- concat <$> mapM (xSuperInit ct className) superInits
  -- maybeSuperCallingCode <- xMaybeSuperInit ct className superInit
  -- let superCallingCode = fromMaybe [] maybeSuperCallingCode
  return $ FragCode (className ++ "__init") (superCallingCode ++ body')

-- super initializers
xMaybeSuperInit :: Counter -> ClassName -> Maybe SuperInit -> IO (Maybe [Target.Stmt])
xMaybeSuperInit _ _ Nothing = return Nothing
xMaybeSuperInit ct className (Just (superclassName, superArgs)) = do
  stmts <- xSuperInit ct className (superclassName, superArgs)
  return $ Just stmts

xSuperInit :: Counter -> ClassName -> SuperInit -> IO [Target.Stmt]
xSuperInit ct className (superclassName, superArgs) = do
  -- TODO: The value below is a placeholder. Implement the function to return the
  -- correct value
  argExprs <- mapM (xExpr ct) superArgs
  let superCall = EXPR $ CALL (NAME (superclassName ++ "__init")) argExprs
  return [superCall]

-- methods
xMethod :: Counter -> ClassName -> Method -> IO Fragment
xMethod ct className (methodName, params, body) = do
  -- TODO: The value below is a placeholder. Implement the function to return the
  -- correct value
  body' <- xBody ct params body
  return $ FragCode (className ++ "__" ++ methodName) body'

-- vtables
xVTable :: Counter -> ClassName -> VTable -> IO Fragment
xVTable ct className vtable = do
  -- TODO: The value below is a placeholder. Implement the function to return the
  -- correct value
  let labels = map (\(cn, method) -> cn ++ "__" ++ method) vtable
  return $ FragLabels (className ++ "__vtable") labels


-----------------------------------------------------------------------------------------
-- Statements
-----------------------------------------------------------------------------------------

{- Conditionals -}
xStmt :: Counter -> Tasty.Stmt -> IO [Target.Stmt]
xStmt ct (SIf expr stmt1 stmt2) = do
  ifTrue    <- freshLabel ct
  ifFalse   <- freshLabel ct
  endIf     <- freshLabel ct
  testCode  <- xExprBool ct expr ifTrue ifFalse
  stmt1Code <- xStmt ct stmt1
  stmt2Code <- xStmt ct stmt2
  return
    $  testCode
    ++ [LABEL ifTrue]
    ++ stmt1Code
    ++ [JUMP endIf, LABEL ifFalse]
    ++ stmt2Code
    ++ [LABEL endIf]

{- Blocks -}
xStmt ct (SBlock stmts) = concat <$> mapM (xStmt ct) stmts

{- Loops -}
xStmt ct (SWhile expr body) = do
  bodyLabel       <- freshLabel ct
  endLabel        <- freshLabel ct
  initialTestCode <- xExprBool ct expr bodyLabel endLabel
  bodyCode        <- xStmt ct body
  repeatTestCode  <- xExprBool ct expr bodyLabel endLabel
  return
    $  initialTestCode
    ++ [LABEL bodyLabel]
    ++ bodyCode
    ++ repeatTestCode
    ++ [LABEL endLabel]

{- Returns -}
xStmt _ (SReturn Nothing) = return [RETURN Nothing]

xStmt ct (SReturn (Just expr)) = do
  exprResult <- xExpr ct expr
  return [RETURN (Just exprResult)]

{- Printing -}
xStmt ct (SPrint IntTy expr) = do
  xStmt ct (SExpr $ ECall "printInt" [expr])

xStmt ct (SPrint BoolTy expr) = do
  xStmt ct (SExpr $ ECall "printBool" [expr])

xStmt ct (SPrint StringTy expr) = do
  xStmt ct (SExpr $ ECall "printString" [expr])

xStmt _ (SPrint ty _) = error $ "Can't print value of type " ++ show ty

{- Assignment -}
xStmt ct (SAssign lhs rhs) = do
  lhsResult <- xExpr ct lhs
  rhsResult <- xExpr ct rhs
  return [ASSIGN lhsResult rhsResult]

{- Expression statements -}
xStmt ct (SExpr expr) = do
  result <- xExpr ct expr
  return [EXPR result]

-----------------------------------------------------------------------------------------
-- Expressions
-----------------------------------------------------------------------------------------
xExpr :: Counter -> Tasty.Expr -> IO Target.Expr

{- Constants -}
xExpr _ (EConstI i) = return $ CONST i
xExpr _ (EConstB True) = return $ CONST 1
xExpr _ (EConstB False) = return $ CONST 0
xExpr _ (EConstS s) = return $ INTS I1 (map (fromIntegral . ord) (s ++ "\0"))
xExpr _ (ENil _) = return $ CONST 0

{- Temporaries -}
xExpr _ (ETemp t) = return $ TEMP t

{- Unary Operators -}
xExpr ct (EUop NegOp expr) = do
  exprResult <- xExpr ct expr
  return $ BINOP (CONST 0) MINUS exprResult

xExpr ct (EUop NotOp expr) = do
  exprResult <- xExpr ct expr
  return $ BINOP (CONST 1) XOR exprResult

xExpr ct (EUop StrLen expr) = xExpr ct (ECall "count" [expr])

{- Binary Operators -}
xExpr ct expr@(EBop expr1 bop expr2)

  -- Arithmetic and logical operators
  | isArithmetic bop || isLogical bop =
    do
      e1 <- xExpr ct expr1
      e2 <- xExpr ct expr2
      return $ BINOP e1 (xBOp bop) e2

  -- Relational operators
  | isComparison bop = do
    t <- freshTemp ct
    let stmt = SIf expr (SAssign (ETemp t) (EConstB True)) (SAssign (ETemp t) (EConstB False))
    stmtResult <- xStmt ct stmt
    return $ ESEQ stmtResult (TEMP t)

  -- Append
  | otherwise = xExpr ct (ECall "append" [expr1, expr2])

{- Ternary operator -}
xExpr ct (ETernary expr1 expr2 expr3) = do
  t <- freshTemp ct
  let s = SIf expr1 (SAssign (ETemp t) expr2) (SAssign (ETemp t) expr3)
  targetStatements <- xStmt ct s
  return $ ESEQ targetStatements (TEMP t)

{- Coalescing -}
xExpr ct (ECoalesce e1 _ e2) = do
  ifNonempty <- freshLabel ct
  ifEmpty <- freshLabel ct
  endIf <- freshLabel ct
  answerTemp <- freshTemp ct
  leftTemp <- freshTemp ct
  testCode <-
    xExprBool
      ct
      (EBop (ETemp leftTemp) NeOp (EConstI 0))
      ifNonempty
      ifEmpty
  expr1Code <- xExpr ct e1
  expr2Code <- xExpr ct e2
  let stmts =
        [ASSIGN (TEMP leftTemp) expr1Code]
          ++ testCode
          ++ [LABEL ifNonempty]
          ++ [ASSIGN (TEMP answerTemp) (MEM I8 (TEMP leftTemp))]
          ++ [JUMP endIf, LABEL ifEmpty]
          ++ [ASSIGN (TEMP answerTemp) expr2Code]
          ++ [LABEL endIf]
  return $ ESEQ stmts (TEMP answerTemp)

{- Calls -}
xExpr ct (ECall name args) = do
  argExprs <- mapM (xExpr ct) args
  return $ CALL (NAME name) argExprs

{- Conversions -}
xExpr ct (EConvert IntTy StringTy expr) = xExpr ct (ECall "intToString" [expr])

xExpr ct (EConvert ty1 (OptionalTy ty2) expr)
  | ty1 == ty2 = xExpr ct (ECall "makeOptional" [expr])

xExpr ct (EConvert StringTy (OptionalTy IntTy) expr) =
  xExpr ct (ECall "stringToOptInt" [expr])

xExpr _ (EConvert ty1 ty2 _) =
  error ("Cannot convert from " ++ show ty1 ++ " to " ++ show ty2)

{- Readline -}
xExpr ct EReadLine = xExpr ct (ECall "readline" [])

{- Record literals -}
xExpr ct (ERecord numFields valueExprs) = do
    -- allocate space for the record, including a dummy field at the start so
    -- record and class fields can be indexed the same way
  t <- freshTemp ct
  let new = ASSIGN (TEMP t) (CALL (NAME "newTable") [CONST (numFields + 1)])

  -- assign values to each field, skipping the 0th field (which would contain
  -- the vtable address in a class)
  let indexedValues = zip [1..] valueExprs
  assignCalls <- mapM (\(idx, rhs) -> xStmt ct (SAssign (EProj (ETemp t) idx) rhs)) indexedValues

  return $ ESEQ (new : concat assignCalls) (TEMP t)

{- Projections -}
xExpr ct (EProj receiverExpr fieldNumber) = do
  -- Get the value of the receiver (which should be its address in memory)
  targetExpr <- xExpr ct receiverExpr

  -- Calculate the field address, skipping the first field (which is the virtual
  -- table in a class and a dummy in a record)
  let fieldAddr = BINOP targetExpr PLUS (CONST (8 * (fieldNumber + 1)))

  -- Dereference the field address
  return $ MEM I8 fieldAddr

{- Static calls -}
xExpr ct (EStaticCall className methodName args) = do
  argExprs <- mapM (xExpr ct) args
  return $ CALL (NAME (className ++ "__" ++ methodName)) argExprs

{- Virtual calls -}
xExpr ct (EInvoke receiverExpr methodNumber args) = do
  -- Get the value of the receiver (which should be its address in memory)
  targetExpr <- xExpr ct receiverExpr

  -- The first field in the receiver is the address of the virtual table
  let tableAddr = MEM I8 targetExpr

  -- Index into the virtual table using the method number
  let fnAddr = BINOP tableAddr PLUS (CONST (8 * methodNumber))

  -- Call the function whose address was found in the virtual table
  argExprs <- mapM (xExpr ct) args
  return $ CALL (MEM I8 fnAddr) argExprs

{- New (object instantiation) -}
xExpr ct (ENew numFields className constructorArgs) = do
  t <- freshTemp ct

  -- Generate a table with room for every field AND the address of the virtual
  -- table
  let new = ASSIGN (TEMP t) (CALL (NAME "newTable") [CONST (numFields + 1)])

  -- Write the address of the virtual table into the 0th field
  let vtable = ASSIGN (MEM I8 (TEMP t)) (NAME (className ++ "__vtable"))
  argExprs <- mapM (xExpr ct) constructorArgs
  let construct = EXPR $ CALL (NAME (className ++ "__init")) (TEMP t : argExprs)
  return $ ESEQ [new, vtable, construct] (TEMP t)

-----------------------------------------------------------------------------------------
-- Helper functions
-----------------------------------------------------------------------------------------

-- | For translating binary operations
xBOp :: BinOp -> Bop
xBOp PlusOp  = PLUS
xBOp TimesOp = MUL
xBOp MinusOp = MINUS
xBOp DivOp   = DIV
xBOp AndOp   = AND
xBOp OrOp    = OR
xBOp op      = error ("Can't directly translate " ++ show op)

-- | For evaluating boolean conditions
xExprBool :: Counter -> Tasty.Expr -> Label -> Label -> IO [Target.Stmt]
xExprBool ct (EBop expr1 bop expr2) ifTrue ifFalse | isComparison bop = do
  expr1Code <- xExpr ct expr1
  expr2Code <- xExpr ct expr2
  let relOp = case bop of
        EqOp -> EQUAL
        NeOp -> NEQUAL
        GtOp -> GREATER
        GeOp -> GREATEREQ
        LtOp -> LESS
        LeOp -> LESSEQ
        _    -> undefined
  return [CJUMP relOp expr1Code expr2Code ifTrue ifFalse]

xExprBool ct (EBop expr1 AndOp expr2) ifTrue ifFalse = do
  midLabel <- freshLabel ct
  step1Code <- xExprBool ct expr1 midLabel ifFalse
  let midCode = [LABEL midLabel]
  step2Code <- xExprBool ct expr2 ifTrue ifFalse
  return $ step1Code ++ midCode ++ step2Code

xExprBool ct (EBop expr1 OrOp expr2) ifTrue ifFalse = do
  midLabel <- freshLabel ct
  step1Code <- xExprBool ct expr1 ifTrue midLabel
  let midCode = [LABEL midLabel]
  step2Code <- xExprBool ct expr2 ifTrue ifFalse
  return $ step1Code ++ midCode ++ step2Code

xExprBool ct (EUop NotOp expr) ifTrue ifFalse = do
  xExprBool ct expr ifFalse ifTrue

xExprBool ct expr ifTrue ifFalse = do
  exprCode <- xExpr ct expr
  t <- freshTemp ct
  return
    [ASSIGN (TEMP t) exprCode, CJUMP EQUAL (TEMP t) (CONST 0) ifFalse ifTrue]
