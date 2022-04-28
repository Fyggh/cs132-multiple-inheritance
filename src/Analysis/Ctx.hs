-- |
-- Module       : Ctx
-- Description  : Typing contexts
-- Maintainer   : CS 132
module Analysis.Ctx
  ( Ctx,
    insertVar,
    insertVars,
    enterFunction,
    lookupVar,
    lookupFunction,
    lookupReturnTy,
    mkInitialCtx,
    updateContextForStmt,
    enterClass,
    lookupCurrentClass,
    isClass,
    lookupVirtualMethod,
    lookupStaticMethod,
    lookupField,
    allClasses,
    getAllFields,
    allVirtualMethods,
    lookupConstructorParams,
    lookupNumFields,
    getSuperclasses,
    inheritsFrom,
  )
where

import qualified Data.IORef
import qualified Data.List
import qualified Data.Map   as Map
import           IR.Hasty   (ClassName, Type (..))
import qualified IR.Hasty   as H
import qualified IR.Tasty   as T
import qualified IR.Temp    as Temp

--
-- Implementation of "typing contexts" (mappings of names to typing information)
--

---------------------------------------------
-- Data Representation                     --
-- (not important for *using* the context) --
---------------------------------------------

-- Describes a class with some number of superclasses, some fields, a
-- constructor, and some methods.
type ClassMapItem = ([H.ClassName], [H.Field], H.Constructor, [H.Method])

data Ctx = Ctx
  { ctxCounter   :: Data.IORef.IORef Integer,
    varMap       :: Map.Map H.Ident (Type, Temp.Temp),
    fnMap        :: Map.Map H.FnName ([Type], Type),
    ctxReturnTy  :: Type,
    ctxClassName :: ClassName,
    ctxClassMap  :: Map.Map ClassName ClassMapItem
  }

insertVar :: Ctx -> H.Ident -> Type -> IO (Ctx, Temp.Temp)
-- insert a Hasty variable with the given type into the typing context.
-- Returns the extended context and a fresh temporary that will be
--   the Tasty-level representation of that Hasty variable.
insertVar ctx x ty = do
  temp <- Temp.mkFreshTemp "t" (ctxCounter ctx)
  let ctx' = ctx {varMap = Map.insert x (ty, temp) (varMap ctx)}
  return (ctx', temp)

insertVars :: Ctx -> [(H.Ident, Type)] -> IO (Ctx, [Temp.Temp])
-- insert a series of Hasty variables with the given types into the typing context.
-- Returns the extended context and a list of temporaries that will be
--   the Tasty-level representations of those Hasty variables.
insertVars ctx [] = return (ctx, [])
insertVars ctx ((x, ty) : rest) = do
  (ctx', temps) <- insertVars ctx rest
  (ctx'', temp) <- insertVar ctx' x ty
  return (ctx'', temp : temps)

insertFunction :: Ctx -> H.FnName -> [(H.Ident, Type)] -> Type -> Ctx
-- Insert information about a function (the function's name,
--   argument list, and return type) into the typing context
-- Returns the extended context.
insertFunction ctx f args res =
  ctx {fnMap = Map.insert f (map snd args, res) (fnMap ctx)}

enterFunction ::
  Ctx -> H.FnName -> [(H.Ident, Type)] -> Type -> IO (Ctx, [Temp.Temp])
-- Get ready to go inside this function (with the given name,
--   argument list, and return type) to start type checking the function body.
-- Records the parameter variables in the typing context (so we can
--   type check uses of those variables) and stores the return type
--   in the context so we can type check any return statements we later encounter.
-- Returns the updated context and a list of fresh temporaries that the
--   lower-level Tasty code will use to refer to the function parameters.
enterFunction ctx _ args res = do
  (ctx', temps) <- insertVars ctx args
  return (ctx' {ctxReturnTy = res}, temps)

lookupVar :: Ctx -> H.Ident -> (Type, Temp.Temp)
-- Get the type of the specified Hasty variable and the temporary
--   that holds the value of this variable.
-- Errors out if this variable is not currently in scope.
lookupVar ctx x = case Map.lookup x (varMap ctx) of
  Just (temp, ty) -> (temp, ty)
  Nothing         -> error ("Unrecognized variable " ++ x)

lookupFunction :: Ctx -> H.Ident -> ([Type], Type)
-- Get the argument types and the return type of the specified
--   Hasty function.
-- Errors out if this function does not exist.
lookupFunction ctx f = case Map.lookup f (fnMap ctx) of
  Just (args, res) -> (args, res)
  Nothing          -> error ("Unrecognized function " ++ f)

lookupReturnTy :: Ctx -> Type
-- Get the return type of the *enclosing* function,
-- which might be VoidTy
--  (This type is set by enterFunction, and is used
--  in type checking return statements.)
lookupReturnTy = ctxReturnTy

--------------------------
-- Class-Related Extras --
--------------------------

enterClass :: Ctx -> ClassName -> Ctx
enterClass ctx className = ctx {ctxClassName = className}

lookupCurrentClass :: Ctx -> ClassName
lookupCurrentClass = ctxClassName

addClassMap :: Ctx -> Map.Map ClassName ClassMapItem -> Ctx
addClassMap ctx classmap = ctx {ctxClassMap = classmap}

isClass :: Ctx -> ClassName -> Bool
-- has a class with the given name been defined?
isClass ctx className = Map.member className (ctxClassMap ctx)

allClasses :: Ctx -> [ClassName]
allClasses ctx = Map.keys (ctxClassMap ctx)

noClassCycles :: Ctx -> Bool
noClassCycles ctx =
  let classmap = ctxClassMap ctx
      noCycles :: [ClassName] -> ClassName -> Bool
      noCycles path c =
        let (superClasses, _, _, _) = classmap Map.! c
         in case superClasses of
              [] -> True
              -- If we have superclasses, none of them may be on the path AND
              -- all no path containing each superclass may have a cycle.
              supers -> null (supers `Data.List.intersect` path) &&
                  all (noCycles (c : path)) supers
   in all (noCycles []) (allClasses ctx)

getClassInfo :: Ctx -> ClassName -> ClassMapItem
getClassInfo ctx classname = case Map.lookup classname (ctxClassMap ctx) of
  Nothing   -> error $ "no such class: " ++ classname
  Just info -> info

getAllFields :: Ctx -> ClassName -> [(H.Ident, Type)]
getAllFields ctx classname =
  let (superClasses, fields, _, _) = getClassInfo ctx classname
   in case superClasses of
        []     -> fields
        supers -> concatMap (getAllFields ctx) supers ++ fields

lookupField :: Ctx -> Type -> H.Ident -> (Integer, Type)
-- Returns the type for the specified field in the specified record or class
--   Also returns an integer n telling us the index of the field
--   *counting from zero* for the first field.

lookupField ctx (ClassTy className) fieldName =
  let fds = getAllFields ctx className
   in case Data.List.elemIndex fieldName (map fst fds) of
        Just n -> (fromIntegral n, snd (fds !! n))
        Nothing -> error $ "Class " ++ className ++ " has no field " ++ fieldName
lookupField _ (RecordTy fields) fieldName =
  case Data.List.elemIndex fieldName (map fst fields) of
    Just n  -> (fromIntegral n, snd (fields !! n))
    Nothing -> error $ "Unknown field " ++ fieldName
lookupField _ ty _ = error $ "Cannot look up field of non-class or non-record type " ++ show ty

type MethodInfo' = ([H.Ident], [H.Ident], Map.Map H.Ident (H.ClassName, [Type], Type))

combineMethods' :: MethodInfo' -> MethodInfo' -> MethodInfo'
combineMethods' (snms1, vnms1, map1) (snms2, vnms2, map2) =
  -- Taking the left-biased union of the maps is intended to favor inheritance
  -- paths in the order that superclasses are listed.
  (snms1 ++ snms2, vnms1 ++ vnms2, map1 `Map.union` map2)

argTypes' :: [(a, b)] -> [b]
argTypes' = map snd

getAllMethods' ::
  Ctx ->
  ClassName ->
  MethodInfo' -- is it static?
getAllMethods' ctx c =
  let (superClasses, _, _, methods) = getClassInfo ctx c
      (inh_snms, inh_vnms, inheritedMap) = case superClasses of
        []     -> ([], [], Map.empty)
        supers -> foldr1 combineMethods' (map (getAllMethods' ctx) supers)

      loop [] snms vnms mp = (snms, vnms, mp)
      loop ((H.Override, m, args, res, _) : rest) snms vnms mp =
        case Map.lookup m mp of
          Nothing -> error $ "Attempt to override non-existent method " ++ m
          Just (c', _, _)
            | c == c' ->
              error $
                "Attempt to override method from same class: "
                  ++ m
                  ++ " in "
                  ++ c
          Just (_, args', res')
            | argTypes' args /= args' || res /= res' ->
              error $ "Attempt to override method with different types: " ++ m
          Just (_, args', res') ->
            if m `elem` vnms
              then loop rest snms vnms (Map.insert m (c, args', res') mp)
              else error $ "Attempt to override static method " ++ m
      loop ((H.Static, m, args, res, _) : rest) snms vnms mp =
        if Map.member m mp
          then error $ "Illegal redefinition of method " ++ m
          else
            loop
              rest
              (snms ++ [m])
              vnms
              (Map.insert m (c, argTypes' args, res) mp)
      loop ((H.Virtual, m, args, res, _) : rest) snms vnms mp =
        if Map.member m mp
          then error $ "Illegal redefinition of method " ++ m
          else
            loop
              rest
              snms
              (vnms ++ [m])
              (Map.insert m (c, argTypes' args, res) mp)
   in loop methods inh_snms inh_vnms inheritedMap

lookupVirtualMethod ::
  Ctx -> ClassName -> H.MethodName -> (Integer, [H.Type], H.Type)
-- Returns the parameter types and return type for the
--   specified virtual method in the specified class.
--   Also returns an integer n telling us the index of the virtual method
--   *counting from zero* for the first virtual method.
lookupVirtualMethod ctx c m =
  let (_, vnms, mp) = getAllMethods' ctx c
   in case Map.lookup m mp of
        Nothing -> error $ "No such method " ++ m ++ " in class " ++ c
        Just (_, argTys, resTy) -> case Data.List.elemIndex m vnms of
          Just n  -> (fromIntegral n, argTys, resTy)
          Nothing -> error $ "Method " ++ m ++ " is not virtual"

lookupStaticMethod ::
  Ctx -> ClassName -> H.Ident -> (ClassName, [H.Type], H.Type)
-- Returns the parameter types and return type for the
--   specified static method in the specified class.
--   Also tells us which class this static method was originally
--      defined in (in case it was inherited).
lookupStaticMethod ctx c m =
  let (snms, _, mp) = getAllMethods' ctx c
   in case Map.lookup m mp of
        Nothing -> error $ "No such method " ++ m ++ " in class " ++ c
        Just (x, y, z) ->
          if m `elem` snms
            then (x, y, z)
            else error $ "Method " ++ m ++ " is virtual, not static"

allVirtualMethods :: Ctx -> ClassName -> [(ClassName, H.Ident)]
allVirtualMethods ctx c =
  let (_, vnms, mp) = getAllMethods' ctx c
   in map (\m -> let (c', _, _) = mp Map.! m in (c', m)) vnms

lookupConstructorParams :: Ctx -> ClassName -> [H.Type]
-- Returns the parameter types for the constructor of the given class
lookupConstructorParams ctx c = case Map.lookup c (ctxClassMap ctx) of
  Nothing -> error $ "No constructor for non-existent class: " ++ c
  Just (_, _, (ps, _, _), _) -> map snd ps

lookupNumFields :: Ctx -> ClassName -> Integer
-- Tells us how many fields there are in objects of the given class
--   (includes all the inherited fields)
lookupNumFields ctx c = fromIntegral $ length (getAllFields ctx c)

getSuperclasses :: Ctx -> ClassName -> [ClassName]
-- Returns the names of the superclasses of the given class, if any
getSuperclasses ctx c = case Map.lookup c (ctxClassMap ctx) of
  Nothing -> error $ "No superclass for non-existent class: " ++ c
  Just (superClasses, _, _, _) -> superClasses

inheritsFrom :: Ctx -> ClassName -> ClassName -> Bool
inheritsFrom _ c1 c2 | c1 == c2 = True
inheritsFrom ctx c1 c2 = case Map.lookup c1 (ctxClassMap ctx) of
  Nothing -> error "Asking inheritsFrom about non-existent class"
  Just ([], _, _, _) -> False
  Just (superClasses, _, _, _) -> any (\super -> inheritsFrom ctx super c2) superClasses

--------------------------------
-- Creating Contexts to Start --
--------------------------------

type Counter = Data.IORef.IORef Integer

mkEmptyCtx :: Counter -> Ctx
-- Make a completely empty context, with no data
-- except for the ubiquitous counter we pass around
-- everywhere so we can generate "fresh" temporaries
-- in a side-effecting way.
mkEmptyCtx ct =
  Ctx
    { ctxCounter = ct,
      varMap = Map.empty,
      fnMap = Map.empty,
      ctxReturnTy = H.VoidTy,
      ctxClassName = "No Class",
      ctxClassMap = Map.empty
    }

mkInitialCtx :: Counter -> H.Program -> Ctx
-- Create a context that is empty except that
--   all the top-level definitions (e.g., functions)
--   have been entered so we can type check recursive
--   calls and calls to functions defined later in the source file.
-- Errors out if there are two top-level definitions with
--   the same name (e.g., two functions with the same name)
mkInitialCtx ct program =
  let ctx0 = addClassMap (mkEmptyCtx ct) (mkClassMap program)

      loop [] = ctx0
      loop (H.DeclFunc f args res _ : rest) =
        let ctx = loop rest
         in if Map.member f (fnMap ctx)
              then error $ "Duplicate definition of function " ++ f
              else insertFunction ctx f args res
      loop (_ : rest) = loop rest
   in if noClassCycles ctx0
        then loop program
        else error "Inheritance goes in a circle!"

mkClassMap :: [H.Declaration] -> Map.Map ClassName ClassMapItem
mkClassMap [] = Map.empty
mkClassMap (H.DeclFunc {} : rest) = mkClassMap rest
mkClassMap (H.DeclClass c x y z w : rest) = Map.insert c (x, y, z, w) (mkClassMap rest)

-----------------------------
-- Random Helper Functions --
-----------------------------

updateContextForStmt :: Ctx -> H.Stmt -> T.Stmt -> Ctx
-- Kind of a hack, but removes a bunch of complexity and boilerplate code
-- in okStmt.
updateContextForStmt ctx (H.SVarDecl x ty _) (T.SAssign (T.ETemp temp) _) =
  ctx {varMap = Map.insert x (ty, temp) (varMap ctx)}
updateContextForStmt ctx _ _ = ctx
