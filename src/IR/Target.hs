module IR.Target
  ( Stmt(..)
  , Expr(..)
  , Fragment(..)
  , Label
  , Rop(..)
  , Bop(..)
  , Bytes(..)
  , notRel
  , commute
  )
where

import           IR.Temp

--------------------------------------------------
-- "Normalized Target Language" Abstract Syntax --
--------------------------------------------------

-- The top-level representation of code is a
--     a list of fragments, each with its own label.
--
data Fragment =
   FragInts   Label Bytes [Integer] -- Labeled sequence of integer constants
 | FragLabels Label [Label]         -- Labeled sequence of (I8) addresses
 | FragCode   Label [Stmt]          -- Labeled piece of code
 deriving (Show, Read)

--
-- Distinguish between bytes and words
--
data Bytes = I1 | I8
 deriving (Show, Read)

--
-- Statements
--

data Stmt =
   SEQ    [Stmt]          -- A sequence of statements as one statement
 | LABEL  Label           -- Emit a label, the destination for a jump
 | JUMP   Label           -- Jump to the given label
 | CJUMP  Rop Expr Expr Label Label -- Jump to labels if true/false
                          -- (both of which can be arbitrarily far away)
 | ASSIGN Expr Expr       -- := operation.  LHS should be a TEMP or MEM
 | EXPR   Expr            -- Evaluate the expression for its side-effects;
                          -- discard the result
 | RETURN (Maybe Expr)    -- Return (with optional return value)
 deriving (Show, Read)

data Expr =
   BINOP Expr Bop Expr    -- Apply the binary operation
 | MEM   Bytes Expr       -- Dereference this address (C's * operator)
                          --   to access the specified number of bytes
 | TEMP  Temp             -- Access a temporary
 | ESEQ  [Stmt] Expr      -- Do the statements for their side-effects, then
                          --    evaluate the expression
 | NAME  Label             -- Address of this label
 | CONST Integer           -- Integer constant
 | CALL Expr [Expr]        -- Function call: address and parameters
 | INTS Bytes [Integer]    -- The address of a global block of memory
                           -- initialized with the given values,
                           -- each value occupying the specified
                           -- number of bytes (useful for arrays & strings)
 deriving (Show, Read)

data Bop =
   PLUS | MINUS | MUL | DIV                     -- (Signed) Arithmetic
 | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR   -- Logical
 deriving (Show, Eq, Read)

data Rop =
   EQUAL | NEQUAL
 | LESS | GREATER | LESSEQ | GREATEREQ      -- Signed comparisons
 | ULESS | UGREATER | ULESSEQ | UGREATEREQ  -- Unsigned comparisons
 deriving (Show, Eq, Read)

--------------------------------------
-- Helper functions for comparisons --
---------------------------------------

notRel :: Rop -> Rop
 -- The negation of the relation.
notRel EQUAL      = NEQUAL
notRel NEQUAL     = EQUAL
notRel LESS       = GREATEREQ
notRel GREATEREQ  = LESS
notRel GREATER    = LESSEQ
notRel LESSEQ     = GREATER
notRel ULESS      = UGREATEREQ
notRel UGREATEREQ = ULESS
notRel UGREATER   = ULESSEQ
notRel ULESSEQ    = UGREATER

commute :: Rop -> Rop
 -- Same comparison, but switching the order of the operands
commute EQUAL      = EQUAL
commute NEQUAL     = NEQUAL
commute LESS       = GREATER
commute GREATER    = LESS
commute LESSEQ     = GREATEREQ
commute GREATEREQ  = LESSEQ
commute ULESS      = UGREATER
commute UGREATER   = ULESS
commute ULESSEQ    = UGREATEREQ
commute UGREATEREQ = ULESSEQ
