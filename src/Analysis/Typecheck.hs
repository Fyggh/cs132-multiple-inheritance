--{-# OPTIONS_GHC -fno-warn-unused-matches #-}

 {-|
Module       : Typecheck
Description  : The Hasty typechecker
Maintainer   : CS 132
-}

module Analysis.Typecheck where

import           Data.Functor ((<&>))
import           IR.Hasty     (Type (..))
import qualified IR.Hasty     as H
import qualified IR.Tasty     as T

import           Analysis.Ctx
--import           Data.Bifunctor

----------------------------------------------------------
-- Important functions to manipulate the Typing Context --
--  (imported from the Ctx module but highlighted here  --
--  for easy reference)                                 --
----------------------------------------------------------

-- WARNING: the functions that insert information into the context
--   are mostly side-effecting (because they also generate
--   fresh temporaries to translate source-level variables)
--   but lookup functions are not.
--
-- SO: insert functions require "<-" lines in do-blocks,
--   but lookup functions require "let ... = ...".

-- insertVar :: Ctx -> H.Ident -> Type -> IO (Ctx, Temp.Temp)
--
--   insert a Hasty variable with the given type into the typing context.
--   Returns the extended context and a fresh temporary that will be
--   the Tasty-level representation of that Hasty variable.

-- insertVars :: Ctx -> [(H.Ident, Type)] -> IO (Ctx, [Temp.Temp])
--
--    inserts a series of Hasty variables with the given types into the
--    typing context. Returns the extended context and a list of temporaries
--    that will be the Tasty-level representations of those Hasty variables.

-- enterFunction :: Ctx -> H.FnName -> [(H.Ident, Type)] -> Type -> IO (Ctx, [Temp.Temp])
--
-- Get ready to go inside this function (with the given name,
--   argument list, and return type) and start type checking the function body.
-- Records the parameter variables in the typing context (so we can
--   type check uses of those variables) and stores the return type
--   in the context so we can type check any return statements we later encounter.
-- Returns the updated context and a list of fresh temporaries that the
--   lower-level Tasty code will use to refer to the function parameters.

-- lookupVar :: Ctx -> H.Ident -> (Type, Temp.Temp)
--
--   Get the type of the specified Hasty variable and the temporary
--     that holds the value of this variable.
--   Errors out if this variable is not currently in scope.

-- lookupFunction :: Ctx -> H.Ident -> ([Type], Type)
--
--   Get the argument types and the return type of the specified
--      Hasty function.
--   Errors out if this function does not exist.

-- lookupReturnTy :: Ctx -> Type
--
--   Get the return type of the *enclosing* function, which might be VoidTy.
--   (This type is set by enterFunction, and is used in type checking
--   return statements.)

-- lookupStaticMethod ::
--   Ctx -> ClassName -> H.Ident -> (ClassName, [H.Type], H.Type)
--
-- Returns the parameter types and return type for the
--   specified static method in the specified class.
--   Also tells us which class this static method was originally
--      defined in (in case it was inherited).

-- lookupNumFields :: Ctx -> ClassName -> Integer
--
-- Tells us how many fields there are in objects of the given class
--   (includes all the inherited fields)

-- lookupConstructorParams :: Ctx -> ClassName -> [H.Type]
--
-- Returns the parameter types for the constructor of the given class

-- lookupVirtualMethod ::
--   Ctx -> ClassName -> H.MethodName -> (Integer, [H.Type], H.Type)
--
-- Returns the parameter types and return type for the
--   specified virtual method in the specified class.
--   Also returns an integer n telling us the index of the virtual method
--   *counting from zero* for the first virtual method.

-- lookupField :: Ctx -> Type -> H.Ident -> (Integer, Type)
--
-- Returns the type for the specified field in the specified record or class
--   Also returns an integer n telling us the index of the field
--   *counting from zero* for the first field.

-----------------------------------------------------------------------------------------
-- Type-checking code
-----------------------------------------------------------------------------------------

------------------------------
-- Programs
------------------------------

-- | Typecheck a Hasty program, and (if successful) return the
--   equivalent Tasty code (with temporaries instead of variables,
--   with a distinction between PlusOp and StringAppendOp,
--   and with all the extra type annotations)
--
--   In practice, this will be called with an initial context
--   that contains information about all the "top-level" definitions
checkProg :: Ctx -> H.Program -> IO T.Program
checkProg ctx = mapM (checkDecl ctx)

------------------------------
-- Declarations
------------------------------

-- | Check a function or class declaration and, if it checks, return the
--   equivalent tasty code.
checkDecl :: Ctx -> H.Declaration -> IO T.Declaration

{- Function declarations -}
checkDecl ctx (H.DeclFunc name hParams resultType hBody) = do
  (tParams, tBody) <- checkBody ctx name hParams resultType hBody
  return $ T.DeclFunc name tParams tBody

{- Class declarations -}
checkDecl ctx (H.DeclClass className hSuperInit _ hConstructor hMethods) = do
  tConstructor <- checkConstructor ctx className hConstructor    -- check constructor
  tMethods <- mapM (checkMethod ctx className) hMethods          -- check methods
  let vms = allVirtualMethods ctx className                      -- build vtable
  return $ T.DeclClassCode className tConstructor tMethods vms   -- make class decl.

{- Helper functions for declarations -}

-- function / method bodies
checkBody :: Ctx -> String -> [H.Parameter] -> Type -> H.Stmt -> IO ([T.Parameter], T.Stmt)
checkBody ctx name hParams resultType hBody = do
  (ctx', tParams) <- enterFunction ctx name hParams resultType
  tBody           <- stmtOK ctx' hBody
  return (tParams, tBody)

-- super initializers
checkSuperInit :: Ctx -> H.ClassName -> H.SuperInit -> IO T.SuperInit
checkSuperInit ctx className (Nothing, hExprs) = do
  let superclasses = getSuperclasses ctx className
  case superclasses of
    [] -> error $ className ++ " does not have a superclass and cannot call super"
    (superName : rest) -> if null rest
      -- Main case: class extends exactly one superclass.
      then checkSuperInitParams ctx superName hExprs
      else error $ "Ambiguous call to super in class " ++ className
        ++ " which uses multiple inheritance."
checkSuperInit ctx className (Just superName, hExprs) = do
  let superclasses = getSuperclasses ctx className
  if superName `elem` superclasses
    then checkSuperInitParams ctx superName hExprs
    else error $ className ++ " does not extend the class " ++ superName
      ++ " and cannot call its constructor."

checkSuperInitParams :: Ctx -> H.ClassName -> [H.Expr] -> IO T.SuperInit
checkSuperInitParams ctx superclassName hExprs = do
  let constructorParamTypes = lookupConstructorParams ctx superclassName
  targs <- checkExprs ctx hExprs constructorParamTypes
  return (superclassName, targs)

-- constructors
checkConstructor :: Ctx -> H.ClassName -> H.Constructor -> IO T.Constructor
checkConstructor ctx className (hParams, hSuperInit, hBody) = do
  let constructorName = className ++ "__init"
  (ctx', tself) <- insertVar ctx "self" (ClassTy className)
  (tParams, tBody) <- checkBody ctx' constructorName hParams VoidTy hBody

  (ctx'', _) <- enterFunction ctx' (className ++ "__init") hParams VoidTy
  tSuperInits <- mapM (checkSuperInit ctx'' className) hSuperInit
  -- let tSuperInits' = map (Data.Bifunctor.second (tself :)) tSuperInits
  -- maybeSuperInit <- checkSuperInit ctx'' className hSuperInit
  -- let tSuperInit = case maybeSuperInit of
  --       Nothing -> Nothing
  --       Just (superclassName, tSuperParams) -> Just (superclassName, T.ETemp tself : tSuperParams)


  -- NOTE: currently tSuperInits do NOT include self as the first parameter.
  return (tself : tParams, tSuperInits, tBody)

-- methods
checkMethod :: Ctx -> H.ClassName -> H.Method -> IO T.Method
checkMethod ctx className (methodKind, methodName, hParams, resultType, hBody) =
  -- TODO: The value below is a placeholder. Implement the function to return
  -- the correct value.
  case methodKind of
    H.Static -> do
          (tParams, tBody) <- checkBody ctx methodName hParams resultType hBody
          return (methodName, tParams, tBody)
    _        -> do
          (ctx', tself) <- insertVar ctx "self" (ClassTy className)
          (tParams, tBody) <- checkBody ctx' methodName hParams resultType hBody
          return (methodName, tself : tParams, tBody)


------------------------------
-- Statements
------------------------------

-- | Check whether the given Hasty statement "makes sense", type-wise.
--   If so, return the corresponding Tasty code.
stmtOK :: Ctx -> H.Stmt -> IO T.Stmt

{- Conditionals -}
stmtOK ctx (H.SIf hexpr1 hexpr2 hexpr3) = do
  texpr1 <- checkExpr ctx hexpr1 H.BoolTy
  texpr2 <- stmtOK ctx hexpr2
  texpr3 <- stmtOK ctx hexpr3
  return $ T.SIf texpr1 texpr2 texpr3

{- While loops -}
stmtOK ctx (H.SWhile hexpr hstmt) = do
  tCondExpr <- checkExpr ctx hexpr BoolTy
  tBodyStmt <- stmtOK ctx hstmt
  return (T.SWhile tCondExpr tBodyStmt)

{- Blocks -}
stmtOK ctx (H.SBlock hstmts     ) = do
  tstmts <- stmtOKs ctx hstmts
  return $ T.SBlock tstmts

{- Returns -}

-- No return value
stmtOK ctx (H.SReturn Nothing) = case lookupReturnTy ctx of
  H.VoidTy -> return $ T.SReturn Nothing
  _        -> error "Expected a value to be return-ed"

-- Return value
stmtOK ctx (H.SReturn (Just hexpr)) = do
  let returnTy = lookupReturnTy ctx
  (exprTy, tExpr) <- synthExpr ctx hexpr
  if returnTy == exprTy
    then return $ T.SReturn (Just tExpr)
    else error $ "Expected return type " ++ show returnTy ++ " does not match actual return type " ++ show exprTy

{- Print -}
stmtOK ctx (H.SPrint  hexpr) = do
  (exprTy, tExpr) <- synthExpr ctx hexpr
  return $ T.SPrint exprTy tExpr

{- Assignments -}

-- Assign to variable
stmtOK ctx (H.SAssign (H.EVar hvar) hexpr2) = do
  let (ty, tvar) = lookupVar ctx hvar
  texpr2 <- checkExpr ctx hexpr2 ty
  return $ T.SAssign (T.ETemp tvar) texpr2

-- Assign to field
stmtOK ctx (H.SAssign (H.EProj hReceiver fieldName) hExpr) = do
  (receiverTy, tReceiver) <- synthExpr ctx hReceiver
  let (fieldNum, fieldTy) = lookupField ctx receiverTy fieldName
  tExpr <- checkExpr ctx hExpr fieldTy
  return $ T.SAssign (T.EProj tReceiver fieldNum) tExpr

-- Other assignments: not supported
stmtOK _ (H.SAssign hexpr1 _) = error $ "Can't assign to " ++ show hexpr1

{- Variable Declarations -}
stmtOK ctx (H.SVarDecl hvar ty hExpr) = do
  tExpr  <- checkExpr ctx hExpr ty
  (_, temp) <- insertVar ctx hvar ty
  return $ T.SAssign (T.ETemp temp) tExpr

{- Statement expressions -}
stmtOK ctx (H.SExpr hExpr) = do
  (_, tExpr) <- synthExpr ctx hExpr
  return $ T.SExpr tExpr

-- | Check whether the given Hasty statements "make sense"
--   as a sequence. If so, return the corresponding Tasty code.
stmtOKs :: Ctx -> [H.Stmt] -> IO [T.Stmt]
stmtOKs _   []               = return []
stmtOKs ctx (hstmt : hstmts) = do
  tstmt <- stmtOK ctx hstmt
    -- we update the context just in case hstmt declared
    -- a new variable that is now in scope and needs
    -- to be added to the context for the remaining statements.
  let ctx' = updateContextForStmt ctx hstmt tstmt
  tstmts <- stmtOKs ctx' hstmts
  return $ tstmt : tstmts

------------------------------
-- Expressions
------------------------------

-- | Check whether the given Hasty expression can have the given type. If so,
--   return the equivalent (annotated) Tasty expression.
checkExpr :: Ctx -> H.Expr -> Type -> IO T.Expr

{- Nil -}
checkExpr _ H.ENil (OptionalTy ty) = return $ T.ENil ty

{- Ternary operator -}
-- Special case to make sure that code like
--     "var n : Int? = (... ? ... : nil);"
-- can type check.
checkExpr ctx (H.ETernary hexpr1 hexpr2 hexpr3) ty = do
  texpr1 <- checkExpr ctx hexpr1 BoolTy
  texpr2 <- checkExpr ctx hexpr2 ty
  texpr3 <- checkExpr ctx hexpr3 ty
  return $ T.ETernary texpr1 texpr2 texpr3

{- Coalescing -}
checkExpr ctx (H.ECoalesce hexpr1 hexpr2) ty = do
  tExpr1 <- checkExpr ctx hexpr1 (OptionalTy ty)
  tExpr2 <- checkExpr ctx hexpr2 ty
  return $ T.ECoalesce tExpr1 ty tExpr2

{- Function calls -}
checkExpr ctx (H.ECall f hargs) ty = do
  let (argTys, _) = lookupFunction ctx f
  targs <- checkExprs ctx hargs argTys
  return $ T.ECall f targs

{- All other expressions: do type inference -}
checkExpr ctx hexpr desiredTy = do
  (ty, texpr) <- synthExpr ctx hexpr
  case convertFromTo ctx ty desiredTy texpr of
    Just texpr' -> return texpr'
    Nothing -> error $ show hexpr ++ " does not have type " ++ show desiredTy

-- | Check that the given list of expressions and types match up, and have the same length.
checkExprs :: Ctx -> [H.Expr] -> [Type] -> IO [T.Expr]
checkExprs _   []               []         = return []
checkExprs ctx (hexpr : hexprs) (ty : tys) = do
  texpr  <- checkExpr ctx hexpr ty
  texprs <- checkExprs ctx hexprs tys
  return $ texpr : texprs
checkExprs _ _  [] = error "Too many arguments"
checkExprs _ [] _  = error "Missing argument"

-----------------------------------------------------------------------------------------
-- Type inference for expressions
-----------------------------------------------------------------------------------------
-- | If the given Hasty expression type checks, return its type and the
--   equivalent (annotated) Tasty Expression.
synthExpr :: Ctx -> H.Expr -> IO (Type, T.Expr)
synthExpr _   (H.EConstI n   ) = return (IntTy, T.EConstI n)
synthExpr _   (H.EConstB b   ) = return (BoolTy, T.EConstB b)
synthExpr _   (H.EConstS s   ) = return (StringTy, T.EConstS s)

{- Variables -}
synthExpr ctx (H.EVar    hvar) = do
  -- Hasty variables like x, y, z
  -- translate to Tasty temporaries like %t1, %t2, %t3
  let (ty, tvar) = lookupVar ctx hvar
  return (ty, T.ETemp tvar)

{- Unary operations -}

-- Unary minus
synthExpr ctx (H.EUop H.NegOp hexpr) = do
  texpr <- checkExpr ctx hexpr IntTy
  return (IntTy, T.EUop T.NegOp texpr)

-- Negation
synthExpr ctx (H.EUop H.NotOp hexpr) = do
  tExpr <- checkExpr ctx hexpr BoolTy
  return (BoolTy, T.EUop T.NotOp tExpr)

-- Addition / string concatenation
synthExpr ctx (H.EBop hexpr1 H.PlusOp hexpr2) = do
  (leftTy, tLeftExpr)   <- synthExpr ctx hexpr1
  (rightTy, tRightExpr) <- synthExpr ctx hexpr2
  case (leftTy, rightTy) of
    (IntTy, IntTy) -> return (IntTy, T.EBop tLeftExpr T.PlusOp tRightExpr)
    (StringTy, StringTy) -> return (StringTy, T.EBop tLeftExpr T.StringAppendOp tRightExpr)
    (_, _) -> error $ "Cannot add two different types: " ++ show leftTy ++ " and " ++ show rightTy

{- Binary operations -}

-- Arithmetic operations
synthExpr ctx (H.EBop hexpr1 hop hexpr2) | H.isArithmetic hop = do
  texpr1 <- checkExpr ctx hexpr1 IntTy
  texpr2 <- checkExpr ctx hexpr2 IntTy
  let top = case hop of
        H.MinusOp -> T.MinusOp
        H.TimesOp -> T.TimesOp
        H.DivOp   -> T.DivOp
        _         -> error "impossible"
  return (IntTy, T.EBop texpr1 top texpr2)

-- Comparisons
synthExpr ctx (H.EBop hexpr1 hop hexpr2) | H.isComparison hop = do
  (leftTy, tLeftExpr)   <- synthExpr ctx hexpr1
  (rightTy, tRightExpr) <- synthExpr ctx hexpr2
  let top = case hop of
        H.EqOp -> T.EqOp
        H.NeOp -> T.NeOp
        H.GtOp -> T.GtOp
        H.GeOp -> T.GeOp
        H.LtOp -> T.LtOp
        H.LeOp -> T.LeOp
        _      -> error "impossible"
  if leftTy == rightTy
    then return (BoolTy,  T.EBop tLeftExpr top tRightExpr)
    else error $ "Cannot compare values of two different types " ++ show leftTy ++ " and " ++ show rightTy

-- Logical operations
synthExpr ctx (H.EBop hexpr1 hop hexpr2) = do
  -- if we get this far, the operator must be && or ||
  texpr1 <- checkExpr ctx hexpr1 BoolTy
  texpr2 <- checkExpr ctx hexpr2 BoolTy
  let top = case hop of
        H.OrOp  -> T.OrOp
        H.AndOp -> T.AndOp
        _       -> error "impossible"
  return (BoolTy, T.EBop texpr1 top texpr2)

{- Ternary operations -}
synthExpr ctx (H.ETernary hexpr1 hexpr2 hexpr3) = do
  -- should succeed if hexpr2's type is convertible to hexpr3's type
  -- or vice versa
  tCond <- checkExpr ctx hexpr1 BoolTy
  (ty2, tExpr2) <- synthExpr ctx hexpr2
  (ty3, tExpr3) <- synthExpr ctx hexpr3
  case convertFromTo ctx ty2 ty3 tExpr2 of
    Just tExpr2' -> return (ty3, T.ETernary tCond tExpr2' tExpr3)
    Nothing -> case convertFromTo ctx ty3 ty2 tExpr3 of
      Just tExpr3' -> return (ty2, T.ETernary tCond tExpr2 tExpr3')
      Nothing      -> error "Mismatched types in ternary expression"

{- Readline -}
synthExpr _   (H.ECall "readLine" []   ) = return (StringTy, T.EReadLine)

{- Function calls -}
synthExpr ctx (H.ECall f hargs) = do
  let (argTys, resultTy) = lookupFunction ctx f
  targs <- checkExprs ctx hargs argTys
  return (resultTy, T.ECall f targs)

{- Conversions -}
synthExpr ctx (H.EConvert toTy hexpr) = do
  (fromTy, texpr) <- synthExpr ctx hexpr
  return (toTy, T.EConvert fromTy toTy texpr)

{- Coalescing -}
synthExpr ctx (H.ECoalesce hexpr1 hexpr2) = do
  (ty, texpr2) <- synthExpr ctx hexpr2
  texpr1       <- checkExpr ctx hexpr1 (OptionalTy ty)
  return (ty, T.ECoalesce texpr1 ty texpr2)

{- Nil -}
synthExpr _ H.ENil = do
  error "Cannot synthesize a type for nil on its own"

{- String length -}
synthExpr ctx (H.EProj hexpr "count") = do
  tExpr <- checkExpr ctx hexpr StringTy
  return (IntTy, T.EUop T.StrLen tExpr)

{- Projections -}
synthExpr ctx (H.EProj hReceiver label) = do
  (receiverTy, tReceiver) <- synthExpr ctx hReceiver
  let (fieldNum, expectedType) = lookupField ctx receiverTy label
  return (expectedType, T.EProj tReceiver fieldNum)

{- Record literals -}
synthExpr ctx (H.ERecord fields) = do
  let (names, hValues) = unzip fields
  (types, tValues) <- mapM (synthExpr ctx) hValues <&> unzip
  return (RecordTy $ zip names types, T.ERecord (toInteger (length fields)) tValues)

------------------------------
-- OOP expressions
------------------------------

{- Static calls -}
synthExpr ctx (H.EStaticCall className methodName hArgs) = do
  let (_, argTypes, returnType) = lookupStaticMethod ctx className methodName
  targs <- checkExprs ctx hArgs argTypes
  return (returnType, T.EStaticCall className methodName targs)

{- Virtual calls -}
synthExpr ctx (H.EInvoke hReceiverExpr methodName hArgs) = do
  -- error "Typechecking for virtual calls is not yet implemented"  -- TODO
  (ClassTy className, tReceiver) <- synthExpr ctx hReceiverExpr
  let (fnIndex, argTypes, returnType) = lookupVirtualMethod ctx className methodName
  targs <- checkExprs ctx hArgs argTypes
  return (returnType, T.EInvoke tReceiver fnIndex (tReceiver : targs))
  -- return (returnType, T.ECall (className ++ "__" ++ methodName) (tReceiver : targs))

{- New (object instantiation) -}
synthExpr ctx (H.ENew className hConstructorArgs) = do
  let constructorParamTypes = lookupConstructorParams ctx className
  let numFields = lookupNumFields ctx className
  targs <- checkExprs ctx hConstructorArgs constructorParamTypes
  return (H.ClassTy className, T.ENew numFields className targs)

-----------------------------------------------------------------------------------------
-- Helper functions
-----------------------------------------------------------------------------------------

-- | We want to convert a Tasty expression from the first type to the second,
--   adding Tasty Convert wrapper(s) corresponding to an *implicit* Hasty
--   conversion. Can we, and what is the resulting Tasty expression?
convertFromTo :: Ctx -> Type -> Type -> T.Expr -> Maybe T.Expr
convertFromTo _ ty1 ty2 texpr | ty1 == ty2 =
    -- If we're trying to convert from a type to itself,
    -- no need to modify the code
  Just texpr
convertFromTo ctx ty1 (OptionalTy ty2) texpr =
    -- If we're try to convert from ty1 to ty2?,
    -- recursively try to convert from ty1 to ty2,
    -- and add a final Convert from ty2 to ty2?
  case convertFromTo ctx ty1 ty2 texpr of
    Just texpr' -> Just $ T.EConvert ty2 (OptionalTy ty2) texpr'
    Nothing     -> Nothing

convertFromTo ctx (ClassTy c1) (ClassTy c2) texpr =
  if inheritsFrom ctx c1 c2 then Just texpr else Nothing

convertFromTo _ _ _ _ =
    -- No other pairs of types correspond to implicit
    -- conversions in Hasty.
  Nothing
