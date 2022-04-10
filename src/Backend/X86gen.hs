{-|
Module       : X86gen
Description  : The x86 code emitter for the compiler -- part of the backend
Maintainer   : CS 132
-}

module Backend.X86gen where

import           Control.Monad (zipWithM)
import qualified IR.Assem      as A
import qualified IR.NormTarget as NT
import qualified IR.Temp       as Temp

------------------------------------------------
-- Support code: generating fresh temporaries --
------------------------------------------------

type Counter = Temp.Counter

-- | Generate a new temporary variable
freshTemp :: Counter -> IO Temp.Temp
-- The input code is already fairly low level and contains temporaries.
-- All the "extra" temporaries we generate when breaking the code
--   down to assembly language will all start with 'g'
freshTemp = Temp.mkFreshTemp "g"

----------------------------------------------------------------
-- Temporaries that correspond to interesting machine registers
----------------------------------------------------------------

rax :: Temp.Temp
rax = Temp.registerTemp "%rax"

rbx :: Temp.Temp
rbx = Temp.registerTemp "%rbx"

cl :: Temp.Temp
cl = Temp.registerTemp "%cl"

rcx :: Temp.Temp
rcx = Temp.registerTemp "%rcx"

rdx :: Temp.Temp
rdx = Temp.registerTemp "%rdx"

rdi :: Temp.Temp
rdi = Temp.registerTemp "%rdi"

rsi :: Temp.Temp
rsi = Temp.registerTemp "%rsi"

rbp :: Temp.Temp
rbp = Temp.registerTemp "%rbp"

rsp :: Temp.Temp
rsp = Temp.registerTemp "%rsp"

r8 :: Temp.Temp
r8 = Temp.registerTemp "%r8"

r9 :: Temp.Temp
r9 = Temp.registerTemp "%r9"

r10 :: Temp.Temp
r10 = Temp.registerTemp "%r10"

r11 :: Temp.Temp
r11 = Temp.registerTemp "%r11"

r12 :: Temp.Temp
r12 = Temp.registerTemp "%r12"

r13 :: Temp.Temp
r13 = Temp.registerTemp "%r13"

r14 :: Temp.Temp
r14 = Temp.registerTemp "%r14"

r15 :: Temp.Temp
r15 = Temp.registerTemp "%r15"

-- | Registers for passing the first six arguments
argumentRegisters :: [Temp.Temp]
argumentRegisters = [rdi, rsi, rdx, rcx, r8, r9]

-- | Registers that the caller must save before the call
callerSaveRegisters :: [Temp.Temp]
callerSaveRegisters = argumentRegisters ++ [rax, r10, r11]

-- | Registers that the callee must save at the start of a call
calleeSaveRegisters :: [Temp.Temp]
calleeSaveRegisters = [rbx, rbp, r12, r13, r14, r15]

-- | All registers
machineRegisters :: [Temp.Temp]
machineRegisters = callerSaveRegisters ++ calleeSaveRegisters ++ [rsp]

-----------------------------------------------------------------------------------------
-- Code Generation for Expressions
-----------------------------------------------------------------------------------------

-- | Returns machine code that puts the value of the given expression into the
--   designated temporary t0.
xExpr :: Counter -> Temp.Temp -> NT.Expr -> IO [A.Instr]
------------------------------------------------
-- Constant values
------------------------------------------------
xExpr _ t0 (NT.CONST i) = do
  return [A.OPER ("\tmovq $" ++ show i ++ ", `d0") [] [t0] Nothing]

------------------------------------------------
-- General temps
------------------------------------------------
xExpr _ t0 (NT.TEMP t) = do
  return [A.MOVE "\tmovq `s0, `d0" t t0]

------------------------------------------------
-- Named temps
------------------------------------------------
xExpr _ t0 (NT.NAME l) =
  return [A.OPER ("\tleaq " ++ l ++ "(%rip), `d0") [] [t0] Nothing]

------------------------------------------------
-- Dereference an 8-bit value
------------------------------------------------
xExpr ct t0 (NT.MEM NT.I8 e1) = do
  t1 <- freshTemp ct
  code1 <- xExpr ct t1 e1
  return $ code1 ++ [A.OPER "\tmovq (`s0), `d0" [t1] [t0] Nothing]

------------------------------------------------
-- Dereference a 64-bit value
------------------------------------------------
xExpr ct t0 (NT.MEM NT.I1 e1) = do
  t1 <- freshTemp ct
  code1 <- xExpr ct t1 e1
  return $
    code1
      ++ [ A.OPER "\tmovb (`s0), %cl" [t1] [rcx] Nothing,
           A.MOVE "\tmovsbq %cl, `d0" rcx t0
         ]

------------------------------------------------
-- Binary logical operations
-- You are free to write separate xExpr cases for BINOP + LSHIFT, BINOP + RSHIFT,
--   etc., but I found it simpler to write one case that could handle shifts
------------------------------------------------
xExpr ct t0 (NT.BINOP e1 bop e2)
  | bop `elem` [NT.LSHIFT, NT.RSHIFT, NT.ARSHIFT] = do
  t1 <- freshTemp ct
  code1 <- xExpr ct t1 e1
  code2 <- xExpr ct rcx e2
  let mnemonic = case bop of
        NT.LSHIFT  -> "shlq"
        NT.RSHIFT  -> "shrq"
        NT.ARSHIFT -> "sarq"
        _          -> undefined
  let moveCode2 = A.MOVE "\tmovq `s0, `d0" t1 t0
  let opCode =
        A.OPER ("\t" ++ mnemonic ++ " %cl, `s1") [rcx, t0] [t0] Nothing
  return $ code1 ++ code2 ++ [moveCode2, opCode]

------------------------------------------------
-- Binary arithmetic operations (except division)
-- You are free to write separate xExpr cases for BINOP + PLUS, BINOP + MINUS,
--   etc., but I found it simpler to write one case that could handle these all.
------------------------------------------------
xExpr ct t0 (NT.BINOP e1 bop e2)
  | bop `elem` [NT.PLUS, NT.MINUS, NT.MUL, NT.AND, NT.OR, NT.XOR] = do
    t1 <- freshTemp ct
    code1 <- xExpr ct t1 e1
    t2 <- freshTemp ct
    code2 <- xExpr ct t2 e2
    let opAssemName =case bop of
          NT.PLUS  -> "addq"
          NT.MINUS -> "subq"
          NT.MUL   -> "imulq"
          NT.AND   -> "andq"
          NT.OR    -> "orq"
          NT.XOR   -> "xorq"
          _        -> undefined
        opCode1 = A.MOVE "\tmovq `s0, `d0" t1 t0
        opCode2 = A.OPER ("\t" ++ opAssemName ++ " `s0, `d0") [t2, t0] [t0] Nothing
    return $ code1 ++ code2 ++ [opCode1, opCode2]

------------------------------------------------
-- Division
-- Division needs more care than +/-/*,
-- because the rules about idivq needing to use %rdx and %rax
------------------------------------------------
xExpr ct t0 (NT.BINOP e1 NT.DIV e2) = do
    t1 <- freshTemp ct
    code1 <- xExpr ct t1 e1
    t2 <- freshTemp ct
    code2 <- xExpr ct t2 e2
    let divCode = [
            A.MOVE "\tmovq `s0, `d0" t1 rax
          , A.OPER "\tcqto" [rax] [rdx] Nothing
          , A.OPER "\tidivq `s0" [t2, rax, rdx] [rax, rdx] Nothing
          , A.MOVE "\tmovq `s0, `d0" rax t0
          ]
    return $ code1 ++ code2 ++ divCode

------------------------------------------------
-- Anything that remains untranslated prints a warning message If you think
-- you're done, this should probably be replaced by a call to error.
------------------------------------------------
xExpr _ _ e = do
  putStrLn ("   unknown xExpr " ++ show e)
  return []

-----------------------------------------------------------------------------------------
-- Code Generation for Statements
-----------------------------------------------------------------------------------------

-- | Translate a list of statements and concatenate the resulting instructions
xStmts :: Counter -> [NT.Stmt] -> IO [A.Instr]
xStmts ct stmts = do
  instrSeqs <- mapM (xStmt ct) stmts
  return $ concat instrSeqs

-- | Generate machine code for a single statement
xStmt :: Counter -> NT.Stmt -> IO [A.Instr]
------------------------------------------------
-- Assignment to a general temporary
------------------------------------------------
xStmt ct (NT.ASSIGN (NT.TEMP t) e) = xExpr ct t e

------------------------------------------------
-- Labels
------------------------------------------------
xStmt _ (NT.LABEL label) = do
  return [A.LABEL ("\n" ++ label ++ ":") label]

------------------------------------------------
-- Jump to label
------------------------------------------------
xStmt _ (NT.JUMP label) = do
  return [A.OPER "\tjmp `j0" [] [] (Just [label])]

------------------------------------------------
-- 64-bit store instruction
------------------------------------------------
-- Note that when writing e2 to address e1, both e1 are e2 are
-- read but not written, i.e., they are both sources and no
--  temporary is a destination!
------------------------------------------------
xStmt ct (NT.ASSIGN (NT.MEM NT.I8 e1) e2) = do
  t1 <- freshTemp ct
  code1 <- xExpr ct t1 e1 --- code to get e1 into some temporary t1
  t2 <- freshTemp ct
  code2 <- xExpr ct t2 e2 --- code to get e2 into some temporary t2
  return (code1 ++ code2 ++ [A.OPER "\tmovq `s1, (`s0)" [t1, t2] [] Nothing])

------------------------------------------------
-- 8-bit store instruction
------------------------------------------------
-- Store the low 8 bits of the 64-bit value e2 at address e1
------------------------------------------------
xStmt ct (NT.ASSIGN (NT.MEM NT.I1 e1) e2) = do
  t1 <- freshTemp ct
  code1 <- xExpr ct t1 e1 --- code to get e1 into some temporary t1
  code2 <- xExpr ct rcx e2 --- code to get e2 into rcx
  return (code1 ++ code2 ++ [A.OPER "\tmovb %cl, (`s0)" [t1, rcx] [] Nothing])

------------------------------------------------
-- Compare-and-jump
------------------------------------------------
xStmt ct (NT.CJUMP rop e1 e2 label1 label2) = do
  t1 <- freshTemp ct
  e1code <- xExpr ct t1 e1
  t2 <- freshTemp ct
  e2code <- xExpr ct t2 e2
  let mnemonic = case rop of
        NT.EQUAL      -> "je"
        NT.NEQUAL     -> "jne"
        NT.LESS       -> "jl"
        NT.GREATER    -> "jg"
        NT.LESSEQ     -> "jle"
        NT.GREATEREQ  -> "jge"
        NT.ULESS      -> "jb"
        NT.UGREATER   -> "ja"
        NT.ULESSEQ    -> "jbe"
        NT.UGREATEREQ -> "jae"
  let compCode = A.OPER "\tcmpq `s0, `s1" [t2, t1] [] Nothing
      jumpCode = A.OPER ("\t" ++ mnemonic ++ " `j0") [] [] (Just [label1, label2])
  return $ e1code ++ e2code ++ [compCode, jumpCode]

------------------------------------------------
-- Calls
------------------------------------------------
xStmt ct (NT.CALL maybeTemp e args) = do

  {- EVALUATE THE FUNCTION -}
  (callASM, fTemps, fCode) <- evalFunction

  {- EVALUATE THE ARGUMENTS -}
  -- XXX: We assume there are no more than 6 arguments!
  -- Generate temps for all the results of evaluating all the arguments
  argTemps <- mapM (const $ freshTemp ct) args
  -- Evaluate all the arguments
  argMoveCode <- zipWithM (xExpr ct) argTemps args

  {- PREPARE THE CALL -}
  -- Move the results of evaluation to the corresponding parameter-registers
  -- Note: We assume that there are no more than 6 arguments to the function
  let regMoveCode = zipWith (A.MOVE "\tmovq `s0, `d0") argTemps argumentRegisters

  {- MAKE THE CALL -}
  let callCode = [A.OPER callASM (fTemps ++ argumentRegisters) callerSaveRegisters Nothing]

  {- HANDLE RETURN VALUE -}
  let returnCode = case maybeTemp of
        Nothing -> []
        Just t  -> [A.MOVE "\tmovq `s0, `d0" rax t]

  return $ fCode ++ concat argMoveCode ++ regMoveCode ++ callCode ++ returnCode

  where evalFunction = case e of
                        NT.NAME functionName -> return ("\tcallq " ++ functionName, [], [])
                        _                    -> do fTemp <- freshTemp ct
                                                   eCode <- xExpr ct fTemp e
                                                   return ("\tcallq *`s0", [fTemp], eCode)

------------------------------------------------
-- Returns
------------------------------------------------
xStmt _ (NT.RETURN Nothing) = return [A.RETURN "\tretq" []]
xStmt ct (NT.RETURN (Just e)) = do
  eCode <- xExpr ct rax e
  return $ eCode ++ [A.RETURN "\tretq" [rax]]

------------------------------------------------
-- Anything that remains untranslated prints a warning message
-- When you think you're done, this should probably be replaced
-- by a call to error.
------------------------------------------------
xStmt _ stmt = do
  putStrLn ("  unknown xStmt " ++ show stmt)
  return []

-----------------------------------------------------------------------------------------
-- Code Generation for Fragments
-----------------------------------------------------------------------------------------

-- | Turns an NT Fragment into the equivalent A.Fragment by just doing
--   instruction-selection on the code parts
xFrag :: Counter -> NT.Fragment -> IO A.Fragment
xFrag ct (NT.FragCode l stmts) = do
  instrs <- xStmts ct stmts
  return $ A.FragCode l instrs
xFrag _ (NT.FragInts l bytes ns) = return $ A.FragInts l bytes ns
xFrag _ (NT.FragLabels l ls) = return $ A.FragLabels l ls

xFrags :: Counter -> [NT.Fragment] -> IO [A.Fragment]
xFrags ct = mapM (xFrag ct)
