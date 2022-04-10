{-|
Module       : NormTarget
Description  : The AST of a low-level "normalized target language"
Maintainer   : CS 132
-}

module IR.NormTarget
  (
    -- * Statements
    Stmt(..)

    -- * Expressions
    --   Instructions that yield a result
  , Expr(..)

    -- * Fragments
  , Fragment(..)

  -- Program jump locations
  , Label

  -- * Relational operators
  , Rop(..)

  -- * Binary (integer) operators
  , Bop(..)

  -- * Data types
  , Bytes(..)
  )
where

-- For the definition of temporary variables
import           IR.Temp
import           IR.Target                         ( Rop(..)
                                                , Bop(..)
                                                , Bytes(..)
                                                )

-----------------------------------------------------------------------------------------
-- Fragments
-----------------------------------------------------------------------------------------

-- | The top-level representation of code. A NormTarget fragment is a list of
--   fragments, each with its own label.
data Fragment =
   FragInts   Label Bytes [Integer] -- ^ Labeled sequence of integer constants
 | FragLabels Label [Label]         -- ^ Labeled sequence of (I8) addresses
 | FragCode   Label [Stmt]          -- ^ Labeled piece of code
 deriving (Show, Read)


--
-- Statements
-----------------------------------------------------------------------------------------

-- | Instructions that can modify memory or jump to a new instruction.
data Stmt =
   -- | The destination for a jump
   LABEL  Label

   -- | Jump to the given label
 | JUMP   Label

   -- | Jump to labels if a comparison is true/false
   --  (but we guarantee this always is *immediately* followed by the last/false
   --   LABEL, so in x86 we only have to translate the "jump if true" part!)
 | CJUMP  Rop Expr Expr Label Label

   -- | Assignment. LHS guaranteed to be a TEMP or MEM
 | ASSIGN Expr Expr

   -- | Function call.
   --
   --   Call a function with 0 or more argument values. Optionally return a
   --   value, and assign that value to a temporary.
 | CALL   (Maybe Temp) Expr [Expr]

   -- | Return (with optional return value)
 | RETURN (Maybe Expr)

 deriving (Show, Read)

-----------------------------------------------------------------------------------------
-- Expressions
-----------------------------------------------------------------------------------------

-- | (64-bit) integer expressions
data Expr =
   BINOP Expr Bop Expr  -- ^ Apply the binary operation
 | MEM   Bytes Expr     -- ^ Dereference this address (C's * operator)
                        --   to access the specified number of bytes
 | TEMP  Temp           -- ^ Access a temporary
 | NAME  Label          -- ^ Address of this label
 | CONST Integer        -- ^ Integer constant
 deriving (Show, Read)
