{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}

module Backend.Regalloc where

import qualified Analysis.Liveness  as Liveness
import qualified Analysis.Makegraph as Makegraph
import qualified Backend.Color      as Color
import qualified Backend.X86gen     as X
import           Data.List          (intercalate)
import qualified Data.Map.Strict    as M
import qualified Data.Map.Strict    as Map
import qualified Data.Set           as S
import qualified IR.Assem           as A
import qualified IR.Temp            as Temp
import qualified System.IO
import qualified Text.Printf
import qualified Utility.FGraph     as FGraph

type Temp = Temp.Temp
type Counter = Temp.Counter

allocFrag :: Counter -> System.IO.Handle -> A.Fragment -> IO A.Fragment
-- Do register allocation for a single A.Fragment.
--   The Counter argument ct is used so we can generate "fresh" variables
--   The Handle is where we write liveness information for debugging purposes.
allocFrag ct h (A.FragCode l instrs) = do

  -- We skip %rsp for obvious reasons, and reserve
  --  %r10 and %r11 use in accessing spilled registers.
  let colorableRegisters = S.fromList
        [ X.rax
        , X.rbx
        , X.rcx
        , X.rdx
        , X.rdi
        , X.rsi
        , X.rbp
        , X.r8
        , X.r9
        , X.r12
        , X.r13
        , X.r14
        , X.r15
        ]

  -- Build the flowgraph
  let (flowInfo@(fgraph, _, _, _)) = Makegraph.instrs2graph instrs

  -- Dataflow analysis to calculate the livein/liveout sets
  -- and build the resulting interference graph. (Also get
  -- a list of all the register-register move instructions we saw.)
  let (livein, igraph, moves)      = Liveness.buildIGraph flowInfo

  -- Get the results of register allocation.
  coloring <- Color.doColor igraph colorableRegisters moves

  -- Write everything we've learned to the .liveness debug file
  System.IO.hPutStrLn h $ "\n\n" ++ l ++ ":"
  mapM_
    (\(n, instr) -> Text.Printf.hPrintf
      h
      "%3d  %-40s   livein: %s\n"
      n
      (fixLeadingWhitespace (A.format instr))
      (intercalate "," (map show (S.elems (livein M.! n))))
    )
    (zip [(0 :: Integer) ..] instrs)
  System.IO.hPutStrLn h $ "\n\n Flow graph for " ++ l ++ "\n"
  System.IO.hPutStrLn h $ FGraph.showFGraph fgraph
  System.IO.hPutStrLn h $ "\n\n Interference graph for " ++ l ++ "\n"
  System.IO.hPutStrLn h $ show igraph
  System.IO.hPutStrLn h $ "\n\n Moves for " ++ l ++ "\n"
  System.IO.hPutStrLn h $ intercalate ", " (map show moves)
  System.IO.hPutStrLn h $ "\n\n Coloring " ++ l ++ "\n"
  System.IO.hPutStrLn h $ unlines
    (map (\(k, v) -> show k ++ " -> " ++ show v) (M.toList coloring))
  System.IO.hFlush h
  -- End of code to write to the .liveness file

  -- Rewrite the assembly code to use the coloring
  --  i.e., replace temporaries with registers
  --  and add spill code if required. Also wraps
  --  beginning & end of code to allocate/deallocate
  --  stack space and to push/pop callee-save registers.
  instrs' <- allocInstrs ct instrs coloring

  -- Return the allocated fragments
  return $ A.FragCode l instrs'

allocFrag _ _ frag =
  -- we don't need to do anything for fragments that are pure data!
  return frag

allocFrags :: Counter -> String -> [A.Fragment] -> IO [A.Fragment]
-- Call allocFrag on all the fragments, giving them all the
--   same liveness file handle to write their debugging info.
allocFrags ct livenessFilename frags = do
  h      <- System.IO.openFile livenessFilename System.IO.WriteMode
  frags' <- mapM (allocFrag ct h) frags
  System.IO.hClose h
  putStrLn $ "Wrote file " ++ livenessFilename
  return frags'


allocInstrs :: Counter -> [A.Instr] -> M.Map Temp Temp -> IO [A.Instr]
-- Given the instructions and the final register-allocation results,
--  rewrite the code appropriately.
allocInstrs _ instrs coloring = do
  let
    -- Calculate information about all the temporaries in the
    -- original (unallocated) code.

    defUse (A.OPER _ src dst _) = (dst, src)
    defUse (A.LABEL _ _       ) = ([], [])
    defUse (A.MOVE _ src dst  ) = ([dst], [src])
    defUse (A.RETURN _ src    ) = ([], src)

    (definedTempLists, usedTempLists) = unzip (map defUse instrs)

    -- Set of all temporaries written to the original instructions
    definedTemps                      = S.fromList (concat definedTempLists)
    -- Set of all temporaries read by the original instructions
    usedTemps                         = S.fromList (concat usedTempLists)
    -- Set of all temporaries mentioned by the original instructions
    allTemps                          = S.union definedTemps usedTemps


    -- Set of all temporaries that must be spilled to the stack
    isStackAllocated t = not (Map.member t coloring)
    spilled = S.filter isStackAllocated allTemps

    -- Which callee-save registers are modified by the allocated code?
    --   (i.e., because they were mentioned in the original instructions,
    --   or something other than itself was assigned
    --   to the callee-save register by the register allocator)
    calleeSavesToSave =
      [ r
      | r <- X.calleeSaveRegisters
      , S.member r usedTemps || r `elem` M.elems (M.delete r coloring)
      ]

    -- Instructions to save/restore the relevant callee-save registers
    --   by pushing/popping their values on the stack
    calleeSaveSaves =
      [ A.OPER "\tpushq `s0" [r] [] Nothing | r <- calleeSavesToSave ]
    calleeSaveRestores =
      [ A.OPER "\tpopq `d0" [] [r] Nothing | r <- reverse calleeSavesToSave ]

    -- MacOS demands the stack pointer be a multiple
    -- of 16 before we can make function calls. One might think this
    -- just means rounding up to a multiple of 16 bytes when we grab
    -- stack space for our local variables, but we have to
    -- account for the 8-byte return address that was pushed
    -- as part of the call to the current code, and the
    -- callee-save registers we've pushed on the stack. Add extra 8
    -- if return address + callee-save registers is not an even
    -- number of 8-byte quantities
    minimumStackSpaceInBytes = 8 * S.size spilled
    stackSpace =
      ((minimumStackSpaceInBytes + 15) `div` 16)
        * 16        -- round up to a multiple of 16
        + (if length calleeSavesToSave `mod` 2 == 0 then 8 else 0)


    -- Enumerate the spilled registers, and map them to offsets
    --  0, 8, 16, ... from the stack pointer.
    offsetMap :: M.Map Temp Integer
    offsetMap = M.fromList (zip (S.elems spilled) [(0 :: Integer), 8 ..])

    rewrite instr =
      let
        (defs, uses)          = defUse instr

        -- uses' is the *stack-allocated* used temporaries in this
        --   instruction (sincde they're the ones that need
        --   nontrivial rewriting)
        uses'                 = filter isStackAllocated uses
        -- similarly, defs' is the *stack-allocated* defined
        --   tenmporaries in this instruction
        defs'                 = filter isStackAllocated defs

        -- Because no x86 instruction has more than two source temporaries
        -- sources or two destination temporaries, we can generate the
        -- necessarily spill code using only two registers, even if
        -- all the inputs/outputs of the instruction have been spilled.
        --
        -- The thing making the code below is a bit convoluted is that
        -- we need to make sure that if a temporary is both a source
        -- and a destination, that we use the same reserved register
        -- for that temporary in the loading code and in the storing code.
        replacementRegisters  = [X.r10, X.r11]

        temps_defed_and_used  = filter (\t -> elem t uses') defs'
        ntemps_defed_and_used = length temps_defed_and_used

        temps_defed_only      = filter (\t -> not (elem t uses')) defs'
        temps_used_only       = filter (\t -> not (elem t defs')) uses'

        regs_for_temps_defed_and_used =
          take ntemps_defed_and_used replacementRegisters
        regs_remaining = drop ntemps_defed_and_used replacementRegisters

        -- A list of (temporary, r10-or-r11) pairs specifying where
        -- spilled temporaries are being loaded into/saved from.
        tempsWithRegs =
          zip temps_defed_and_used regs_for_temps_defed_and_used
            ++ zip temps_defed_only regs_remaining
            ++ zip temps_used_only  regs_remaining

        -- Map each temporary either to the appropriate r10 or r11
        -- (if the temporary was spilled), or to the register
        -- chosen by the register allocator (if not)
        updateTemp t = case lookup t tempsWithRegs of
          Just r  -> r
          Nothing -> case Map.lookup t coloring of
            Just r  -> r
            Nothing -> t   -- shouldn't happen.

        updateTemps     = map updateTemp

        -- Spill-related code to put before the instruction
        -- and after the instruction, respectively.
        preMovesNeeded  = map (\t -> (t, updateTemp t)) uses'
        postMovesNeeded = map (\t -> (t, updateTemp t)) defs'

        preMoveInstrs   = map
          (\(t, r) -> A.OPER
            ("\tmovq " ++ show (offsetMap M.! t) ++ "(%rsp), `d0")
            []
            [r]
            Nothing
          )
          preMovesNeeded

        postMoveInstrs = map
          (\(t, r) -> A.OPER
            ("\tmovq `s0, " ++ show (offsetMap M.! t) ++ "(%rsp)")
            [r]
            []
            Nothing
          )
          postMovesNeeded

        -- Finally, we put everything together and rewrite
        -- instruction instr to handle spilling
        --
        -- If it's a RETURN, we also insert the "pop the stack
        --  and the callee-save registers" right before the retq
        --  instruction.
        instrs' = case instr of
          A.OPER asm src dst jumps ->
            [A.OPER asm (updateTemps src) (updateTemps dst) jumps]
          A.LABEL _ _        -> [instr]
          A.MOVE asm src dst -> [A.MOVE asm (updateTemp src) (updateTemp dst)]
          A.RETURN asm src ->
            [A.OPER ("\taddq $" ++ show stackSpace ++ ", %rsp") [] [] Nothing]
              ++ calleeSaveRestores
              ++ [A.RETURN asm (updateTemps src)]
      in
        preMoveInstrs ++ instrs' ++ postMoveInstrs

  -- Emit a few comments about spilled registers into
  -- the generated assembly code.
  let debugInstrs = map
        (\(t, i) -> A.OPER
          ("\t# " ++ show t ++ " lives at " ++ show i ++ "(%rsp)")
          []
          []
          Nothing
        )
        (M.toList offsetMap)

  -- Finally return the updated instruction sequence, starting with the
  -- code to save the necessary callee-save registers and allocate
  -- stack space, followed by the spill-info comments, and finally
  -- the instructions rewritten according to the given coloring.
  return
    $  calleeSaveSaves
    ++ [A.OPER ("\tsubq $" ++ show stackSpace ++ ", %rsp") [] [] Nothing]
    ++ debugInstrs
    ++ concatMap rewrite instrs

-- Little helper function used when putting line numbers
-- before the assembly instructions in the .liveness file.
fixLeadingWhitespace :: String -> String
fixLeadingWhitespace ('\n' : rest) = fixLeadingWhitespace rest
fixLeadingWhitespace ('\t' : rest) = "  " ++ rest
fixLeadingWhitespace s             = s
