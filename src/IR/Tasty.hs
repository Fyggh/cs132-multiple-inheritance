{-|
Module       : Tasty
Description  : The AST of a Tasty program
Maintainer   : CS 132
-}

module IR.Tasty (
       Type (..)
     , FnName
     , UnaryOp (..)
     , BinOp (..)
     , isComparison
     , isArithmetic
     , isLogical
     , isString
     , Expr (..)
     , Stmt (..)
     , Declaration (..)
     , Constructor
     , Method
     , ClassName
     , MethodName
     , Parameter
     , SuperInit
     , VTable
     , Program
)
where

-----------------
-- Identifiers --
-----------------

import           IR.Hasty (Type (..))
import           IR.Temp

type Ident      = String
type FnName     = String
type ClassName  = String
type MethodName = String

-----------------------------
-- Unary Integer Operators --
-----------------------------

data UnaryOp = NegOp             --  (signed) integer negation
             | NotOp             --  Logical negation (!)
             | StrLen            -- String length
     deriving (Eq, Show)

------------------------------
-- Integer Binary Operators --
------------------------------

data BinOp = PlusOp | TimesOp | MinusOp | DivOp      -- (signed) integer arithmetic
           | EqOp | NeOp | GtOp | GeOp | LtOp | LeOp -- int comparisons
           | AndOp | OrOp                            -- && and ||
           | StringAppendOp                          -- String append
     deriving (Eq, Show)

isComparison :: BinOp -> Bool
isComparison op = op `elem` [EqOp, NeOp, GtOp, GeOp, LtOp, LeOp]

isArithmetic :: BinOp -> Bool
isArithmetic op = op `elem` [PlusOp, MinusOp, TimesOp, DivOp]

isLogical :: BinOp -> Bool
isLogical op = op `elem` [AndOp, OrOp]

isString :: BinOp -> Bool
isString op = op  == StringAppendOp

-----------------
-- Expressions --
-----------------

data Expr = EConstI  Integer                        -- nonnegative integer
          | EConstB  Bool                           -- boolean constant
          | ETemp    Temp                           -- variable
          | EUop     UnaryOp Expr                   -- unary operation
          | EBop     Expr BinOp Expr                -- binary operation
          | ETernary Expr Expr Expr                 -- ? : operator
          | ECall FnName [Expr]                     -- Function call
          | EConstS  String                         -- String constant
          | EConvert Type Type Expr                 -- Convert(ty1, ty2, e)
          | EReadLine                               -- ReadLine
          | ECoalesce Expr Type Expr                -- e1 ?? ty e2
          | ENil Type                               -- nil(ty)
          | EProj Expr Integer                      -- nth field of an object
          | EStaticCall ClassName MethodName [Expr] -- Class.method(args)
          | EInvoke Expr Integer [Expr]             -- nth virtual method of an object
          | ENew Integer ClassName [Expr]           -- construct a new class instance
                                                    --   with the given number of fields
          | ERecord Integer [Expr]                  -- create a new record w/ the given
                                                    --   number of fields
          deriving (Eq, Show)

----------------
-- Statements --
----------------

data Stmt =
   SIf     Expr Stmt Stmt        -- An if/else statement [nonzero == true]
 | SBlock  [Stmt]                -- A block/sequence of statements
 | SWhile Expr Stmt              -- while loop (not a do loop!)
 | SReturn (Maybe Expr)          -- function return
 | SPrint  Type Expr             -- Display the value
 | SAssign Expr Expr             -- A single assignment, l-value and r-value
 | SExpr   Expr                  -- Expression executed for its side-effects
         deriving (Eq, Show)

----------------
-- Classes    --
----------------

-- Method name, parameters, body
type Method = (Ident, [Parameter], Stmt)

-- A super initializer is a pair of a class name and a list of expressions. The
-- expressions will be passed to the constructor of the specified classname,
-- which represents the superclass. Generic calls to `super` will be translated
-- into specific classnames by the typechecker.
type SuperInit = (ClassName, [Expr])

-- Constructor parameters, superclass and superclass-constructor args (if any), body
type Constructor = ([Parameter], [SuperInit], Stmt)

-- A vtable is a list of classname-methodname pairs.
type VTable = [(ClassName,MethodName)]

-------------
-- Program --
-------------

type Parameter = Temp

data Declaration =
   DeclFunc FnName [Parameter] Stmt
 | DeclClassCode ClassName Constructor [Method] VTable
       -- At the Tasty level, we only need to keep around the parts
       --    of the class that turn into Target.Fragment's
       -- The final list is the data we need for the virtual method table,
       --    a list of all the virtual methods (including inherited methods)
       --    in this class, paired with the class where the virtual method
       --    was inherited from.
     deriving (Eq, Show)

type Program = [Declaration]
