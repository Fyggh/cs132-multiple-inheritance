{-|
Module       : Hasty
Description  : Intermediate representation for Hasty
Maintainer   : CS 132
-}

module IR.Hasty where

-----------------
-- Identifiers --
-----------------

type Ident      = String
type FnName     = String
type FieldName  = String
type MethodName = String
type ClassName  = String

-----------------------------
-- Unary Integer Operators --
-----------------------------

data UnaryOp = NegOp          --  (signed) integer negation
             | NotOp          --  Logical negation (!)
     deriving (Eq, Show)

------------------------------
-- Integer Binary Operators --
------------------------------

data BinOp = PlusOp                                  -- int addition or string append
           | TimesOp | MinusOp | DivOp               -- (signed) integer arithmetic
           | EqOp | NeOp | GtOp | GeOp | LtOp | LeOp -- int comparisons
           | AndOp | OrOp                            -- && and ||
     deriving (Eq, Show)

isComparison :: BinOp -> Bool
isComparison op = op `elem` [EqOp, NeOp, GtOp, GeOp, LtOp, LeOp]

isArithmetic :: BinOp -> Bool
isArithmetic op = op `elem` [PlusOp, MinusOp, TimesOp, DivOp]

-----------------
-- Expressions --
-----------------

data Expr = EConstI     Integer                 -- nonnegative integer
          | EConstB     Bool                    -- boolean constant
          | EConstS     String                  -- String constant
          | EVar        Ident                   -- variable
          | EUop        UnaryOp Expr            -- unary operation
          | EBop        Expr BinOp Expr         -- binary operation
          | ETernary    Expr Expr Expr          -- ? : operator
          | ECall       FnName [Expr]           -- Function call
          | EConvert    Type Expr               -- e.g., String(n)
          | ECoalesce   Expr Expr               -- e1 ?? e2
          | ENil                                -- nil
          | EProj       Expr Ident              -- e.field or e.count
          | EStaticCall ClassName Ident [Expr]  -- Class.method(args)
          | EInvoke     Expr Ident [Expr]       -- e.method(args)
          | ENew        ClassName [Expr]        -- construct new class instance
          | ERecord     [(Ident, Expr)]         -- a literal record value
          deriving (Eq, Show)

----------------
-- Statements --
----------------

data Stmt =
   SIf      Expr Stmt Stmt        -- An if/else statement [nonzero == true]
 | SBlock   [Stmt]                -- A block/sequence of statements
 | SWhile   Expr Stmt             -- while loop (not a do loop!)
 | SReturn  (Maybe Expr)          -- function return
 | SPrint   Expr                  -- Display the value
 | SAssign  Expr Expr             -- A single assignment, l-value and r-value
 | SVarDecl Ident Type Expr       -- Local variable declaration (var x : T = e)
 | SExpr    Expr                  -- expr;   Executed for its side effects
         deriving (Eq, Show)

-----------
-- Types --
-----------

data Type =
   IntTy                    -- Int
 | BoolTy                   -- Bool
 | StringTy                 -- String
 | OptionalTy Type          -- T ?
 | VoidTy                   -- Void
 | ClassTy ClassName        -- Class type
 | RecordTy [(Ident, Type)] -- Record type
     deriving (Eq, Show)

-------------
-- Classes --
-------------

-- A field has a name and a type
type Field = (Ident, Type)

-- A method has a kind, a name, a list of parameters, a result type, and a body
type Method = (MethodKind, Ident, [Parameter], Type, Stmt)

data MethodKind = Static | Virtual | Override
     deriving (Show, Eq)

-- A super initializer is an optional list of expressions. If the value is not
-- Nothing, then the list of expressions will be passed as arguments to the
-- superclass constructor.
type SuperInit = Maybe [Expr]

-- A constructor has a list of parameters, an optional superclass initializer
-- (which is a list of arguments for the super-class constructor), and a body.
type Constructor = ([Parameter], SuperInit, Stmt)

-------------
-- Program --
-------------

-- A parameter has a name and a type
type Parameter = (Ident, Type)

data Declaration =
   -- A function has a name, a list of parameters, a result type, and a body.
   DeclFunc FnName [Parameter] Type Stmt

   -- A class has a name an optional superclass name, a list of declared fields,
   -- a constructor, and a list of methods
 | DeclClass ClassName (Maybe ClassName) [Field] Constructor [Method]
     deriving (Eq, Show)

type Program = [Declaration]
