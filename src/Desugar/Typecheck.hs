module Desugar.Typecheck where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative

import Data.Maybe
import Data.Foldable (foldlM)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Syntax.Expression
import Syntax.Type
import Syntax.Module
import Syntax.Name
import Syntax.Location

import Desugar.Unify
import Desugar.Rename

import qualified Core.Module as C
import qualified Core.Expression as C

import Debug.Trace

data Environment = Environment
  { environmentTypeMap  :: Map QCoreName (PolyType CoreName) -- Variables
  , environmentKindMap  :: Map QCoreName (Kind CoreName)     -- Kind Variables
  , environmentRenaming :: RenameMap
  }

data TypecheckError = UnboundVariable QCoreName
                    | TCUnifyError UnifyError
                    | InTypechecking (Expression CoreName) TypecheckError
                    | UnknownPrimitive String
                    deriving (Show)

type TypecheckMonad = ReaderT Environment (ExceptT TypecheckError (StateT Int UnifyMonad))

generateName :: TypecheckMonad CoreName
generateName = do
  n <- get
  modify (+ 1)
  return $ CoreName n "a"

runTypecheckMonad :: Environment -> TypecheckMonad a -> State Int (Either TypecheckError a)
runTypecheckMonad env m = do
  c      <- get
  let v = runUnifyMonad . flip runStateT c . runExceptT . flip runReaderT env $ m
  case v of
    Left e             -> return (Left $ TCUnifyError e)
    Right (Left e, _)  -> return (Left e)
    Right (Right r, c) -> do
      put c
      return (Right r)

liftUnify :: UnifyMonad a -> TypecheckMonad a
liftUnify m = do
  r <- lift.lift.lift $ (Right <$> m) `catchError` (\e -> return (Left e))
  case r of
    Right r -> return r
    Left e -> throwError (TCUnifyError e)

localBind :: CoreName -> PolyType CoreName -> TypecheckMonad a -> TypecheckMonad a
localBind x t = local $ \s -> s { environmentTypeMap = Map.insert (QName [] VariableName x) t (environmentTypeMap s) }

localBindMany :: Map CoreName (PolyType CoreName) -> TypecheckMonad a -> TypecheckMonad a
localBindMany bs = local $ \s -> s { environmentTypeMap = Map.union (Map.mapKeys (QName [] VariableName) bs) (environmentTypeMap s) }

globalBindMany :: Map QCoreName (PolyType CoreName) -> TypecheckMonad a -> TypecheckMonad a
globalBindMany bs = local $ \s -> s { environmentTypeMap = Map.union bs (environmentTypeMap s) }

getPrimitive :: NameSpace -> String -> TypecheckMonad QCoreName
getPrimitive ns s = do
  p <- Map.lookup (QName ["Primitive"] ns (UserName s)) . environmentRenaming <$> ask
  case p of
    Just (RenameGlobal [p]) -> return p
    _                       -> throwError $ UnknownPrimitive s

instanciateType :: PolyType CoreName -> TypecheckMonad (MonoType CoreName)
instanciateType (PolyType as t) = do
    subst <- Map.fromList <$> forM (Set.toList as) (\a -> (,) a <$> generateName)
    let applySubstitution (TyVariable v)      = case Map.lookup v subst of
          Just a  -> TyVariable a
          Nothing -> TyVariable v
        applySubstitution (TyApplication a b) = TyApplication (applySubstitution a) (applySubstitution b)
        applySubstitution (TyConstant c)      = TyConstant c
    applySubstitution <$> (liftUnify $ substituteType t)

environmentVariables :: TypecheckMonad (Set CoreName)
environmentVariables = Set.unions . (freePolyTypeVariables <$>) . Map.elems . environmentTypeMap <$> ask

typecheckExpression :: Expression CoreName -> TypecheckMonad C.Expression
typecheckExpression e = typecheckExpression' (delocate e) `catchError` (\err -> throwError $ InTypechecking e err)
  where
    typecheckExpression' (EInteger i)                                 = do
      tyInt <- TyConstant <$> getPrimitive TypeConstructorName "Int"
      return $ C.Expression tyInt (C.EInteger i)
    typecheckExpression' (EChar c)                                    = do
      tyChar <- TyConstant <$> getPrimitive TypeConstructorName "Char"
      return $ C.Expression tyChar (C.EChar c)
    typecheckExpression' (EVariable x) = do
      emap <- environmentTypeMap <$> ask
      t <- Map.lookup x . environmentTypeMap <$> ask
      case t of
        Nothing -> throwError $ UnboundVariable x
        Just t  -> do
          ty <- instanciateType t
          return $ C.Expression ty (C.EVariable x)
    typecheckExpression' (EApplication f t)                           = do
      fTy   <- typecheckExpression f
      tTy   <- typecheckExpression t
      tau   <- generateName
      sigma <- generateName
      liftUnify $ unifyType (C.expressionType tTy) (TyVariable tau)
      tyArrow <- TyConstant <$> getPrimitive TypeConstructorName "->"
      liftUnify $ unifyType (C.expressionType fTy) (makeTypeApplication tyArrow [TyVariable tau, TyVariable sigma])
      return $ C.Expression (TyVariable sigma) (C.EApplication fTy tTy)
    typecheckExpression' (ELambda x e)                                = do
      tau     <- generateName
      tyArrow <- TyConstant <$> getPrimitive TypeConstructorName "->"
      eTy     <- localBind x (PolyType Set.empty (TyVariable tau)) $ typecheckExpression e
      return $ C.Expression (makeTypeApplication tyArrow [TyVariable tau, C.expressionType eTy]) (C.ELambda x eTy)
    typecheckExpression' (ETuple es)                                  = do
      ts <- mapM typecheckExpression es
      tyTuple <- TyConstant <$> getPrimitive TypeConstructorName (replicate (length es - 1) ',')
      return $ C.Expression (makeTypeApplication tyTuple (C.expressionType <$> ts)) (C.ETuple ts)
    typecheckExpression' (EIf c a b)                                  = do
      cTy <- typecheckExpression c
      aTy <- typecheckExpression a
      bTy <- typecheckExpression b
      tyBool <- TyConstant <$> getPrimitive TypeConstructorName "Bool"
      liftUnify $ unifyType (C.expressionType cTy) tyBool
      liftUnify $ unifyMonoType (C.expressionType aTy) (C.expressionType bTy)
      return $ C.Expression (C.expressionType aTy) (C.EIf cTy aTy bTy)
    typecheckExpression' (ELet bs e)                                  = do
      bts <- typecheckBindings [] bs
      eTy <- localBindMany (Map.map fst bts) $ typecheckExpression e
      return $ C.Expression (C.expressionType eTy) (C.ELet bts eTy)
    typecheckExpression' (EListCase e nil x xs r)                     = do
      eTy    <- typecheckExpression e
      tau    <- generateName
      tyList <- TyConstant <$> getPrimitive TypeConstructorName "[]"
      liftUnify $ unifyType (C.expressionType eTy) (TyApplication tyList (TyVariable tau))
      nilTy  <- typecheckExpression nil
      rTy    <- localBind x (PolyType Set.empty $ TyVariable tau)
              $ localBind xs (PolyType Set.empty $ TyApplication tyList (TyVariable tau))
              $ typecheckExpression r
      liftUnify $ unifyType (C.expressionType nilTy) (C.expressionType rTy)
      return $ C.Expression (C.expressionType nilTy) (C.EListCase eTy nilTy x xs rTy)

typecheckDeclaration :: Declaration CoreName -> TypecheckMonad C.Declaration
typecheckDeclaration (Declaration e)             = C.Declaration <$> typecheckExpression e
typecheckDeclaration (PrimitiveDeclaration prim) = return $ C.PrimitiveDeclaration prim

primitiveType :: PrimitiveDeclaration -> TypecheckMonad (MonoType CoreName)
primitiveType p 
  | p `elem` [FromIntegerDeclaration, NegateDeclaration]
      = do
    tyInt   <- TyConstant <$> getPrimitive TypeConstructorName "Int"
    tyArrow <- TyConstant <$> getPrimitive TypeConstructorName "->"
    return $ makeTypeApplication tyArrow [tyInt, tyInt]
  | p `elem` [ AddDeclaration, SubDeclaration, MulDeclaration, DivDeclaration, RemDeclaration
             , LTDeclaration, LEDeclaration, GTDeclaration, GEDeclaration, EQDeclaration, NEDeclaration]
      = do
    tyInt   <- TyConstant <$> getPrimitive TypeConstructorName "Int"
    tyArrow <- TyConstant <$> getPrimitive TypeConstructorName "->"
    return $ makeTypeApplication tyArrow [tyInt, makeTypeApplication tyArrow [tyInt, tyInt]]
  | p `elem` [ AndDeclaration, OrDeclaration]
      = do
    tyBool  <- TyConstant <$> getPrimitive TypeConstructorName "Bool"
    tyArrow <- TyConstant <$> getPrimitive TypeConstructorName "->"
    return $ makeTypeApplication tyArrow [tyBool, makeTypeApplication tyArrow [tyBool, tyBool]]
  | p == BindDeclaration
      = do
    a       <- TyVariable <$> generateName
    b       <- TyVariable <$> generateName
    tyIO    <- TyConstant <$> getPrimitive TypeConstructorName "IO"
    tyArrow <- TyConstant <$> getPrimitive TypeConstructorName "->"
    return $ makeTypeApplication tyArrow [TyApplication tyIO a, makeTypeApplication tyArrow [makeTypeApplication tyArrow [a, TyApplication tyIO b], TyApplication tyIO b]]
  | p == ReturnDeclaration
      = do
    a       <- TyVariable <$> generateName 
    tyIO    <- TyConstant <$> getPrimitive TypeConstructorName "IO"
    tyArrow <- TyConstant <$> getPrimitive TypeConstructorName "->"
    return $ makeTypeApplication tyArrow [a, TyApplication tyIO a]
  | p == ErrorDeclaration
      = do
    a       <- TyVariable <$> generateName 
    tyList  <- TyConstant <$> getPrimitive TypeConstructorName "[]"
    tyChar  <- TyConstant <$> getPrimitive TypeConstructorName "Char"
    tyArrow <- TyConstant <$> getPrimitive TypeConstructorName "->"
    return $ makeTypeApplication tyArrow [TyApplication tyList tyChar, a]
  | p == PutCharDeclaration
      = do
    tyUnit  <- TyConstant <$> getPrimitive TypeConstructorName "()"
    tyChar  <- TyConstant <$> getPrimitive TypeConstructorName "Char"
    tyIO    <- TyConstant <$> getPrimitive TypeConstructorName "IO"
    tyArrow <- TyConstant <$> getPrimitive TypeConstructorName "->"
    return $ makeTypeApplication tyArrow [tyChar, TyApplication tyIO tyUnit]

typecheckBindings :: ModuleName -> DeclarationMap CoreName -> TypecheckMonad C.DeclarationMap
typecheckBindings md bs = do
    let bgs = reverse $ topologicalSort (Set.fromList $ Map.keys mBindings) []
    Map.mapKeys (\(QName _ VariableName k) -> k) <$> foldlM
      (\bts bg -> (Map.union bts <$>) $ globalBindMany (Map.map fst bts) $ (Map.union bts) <$> typecheckBindings' bg)
      Map.empty
      bgs
  where
    mBindings = Map.mapKeys (QName md VariableName) bs

    dependenciesMapStep :: Map QCoreName (Set QCoreName) -> Map QCoreName (Set QCoreName)
    dependenciesMapStep mp = Map.mapWithKey (\k v -> Set.union v $ Set.unions $ fromJust . flip Map.lookup mp <$> Set.toList v) mp

    makeDependenciesMap :: Int -> Map QCoreName (Set QCoreName) -> Map QCoreName (Set QCoreName)
    makeDependenciesMap 0 = id
    makeDependenciesMap n = makeDependenciesMap (n `div` 2) . dependenciesMapStep

    dependenciesMap :: Map QCoreName (Set QCoreName)
    dependenciesMap = makeDependenciesMap (Map.size mBindings) (Map.map (Set.intersection (Map.keysSet mBindings) . declarationFreeVariables) mBindings)

    reverseDependenciesMap :: Map QCoreName (Set QCoreName)
    reverseDependenciesMap = Map.foldWithKey (\k -> flip . Set.fold . Map.update $ Just . Set.insert k) (Map.map (const Set.empty) mBindings) dependenciesMap

    topologicalSort :: Set QCoreName -> [Set QCoreName] -> [Set QCoreName]
    topologicalSort st acc
      | Set.null st = acc
      | otherwise   =
          let elem       = Set.findMin st
              allElemDep = fromJust $ Map.lookup elem dependenciesMap
              revElemDep = fromJust $ Map.lookup elem reverseDependenciesMap
              elemSet    = Set.insert elem $ Set.intersection allElemDep revElemDep
              elemDep    = allElemDep `Set.difference` elemSet
              acc'       = topologicalSort elemDep acc
            in
          topologicalSort (Set.delete elem $ st `Set.difference` allElemDep) (elemSet : acc')

    typecheckBindings' :: Set QCoreName -> TypecheckMonad (Map QCoreName (PolyType CoreName, C.Declaration))
    typecheckBindings' st = do
      let xs = Set.toList st
          es = fromJust . flip Map.lookup mBindings <$> xs
      ts <- forM xs (const $ TyVariable <$> generateName)
      esTy <- globalBindMany (Map.fromList $ zip xs (PolyType Set.empty <$> ts)) $ forM (zip es ts) $ \(e, t) -> do
        eTy <- typecheckDeclaration e
        case eTy of
          C.Declaration e             -> liftUnify $ unifyType t (C.expressionType e)
          C.PrimitiveDeclaration prim -> do
            pTy <- primitiveType prim
            liftUnify $ unifyType t pTy
        return $ eTy
      ts    <- liftUnify $ substituteType `mapM` ts
      freeG <- environmentVariables
      let tvs = uncurry PolyType <$> zip ((`Set.difference` freeG) . freeTypeVariables <$> ts) ts
      return $ Map.fromList $ zip xs (zip tvs esTy)

typecheckDataConstructor :: ModuleName -> MonoType CoreName -> Set CoreName -> DataConstructor CoreName -> TypecheckMonad (Map QCoreName (PolyType CoreName))
typecheckDataConstructor md ty tvs (DataConstructor n ts) = do
  tyArrow <- TyConstant <$> getPrimitive TypeConstructorName "->"
  return $ Map.singleton (QName md ConstructorName n) $ PolyType tvs $ foldr (\a b -> makeTypeApplication tyArrow [a, b]) ty ts

typecheckDataDeclaration :: ModuleName -> QCoreName -> DataDeclaration CoreName -> TypecheckMonad (Map QCoreName (PolyType CoreName))
typecheckDataDeclaration md n (DataDeclaration tvs dcs) = do
  let ty = makeTypeApplication (TyConstant n) (TyVariable <$> tvs)
  Map.unions <$> typecheckDataConstructor md ty (Set.fromList tvs) `mapM` dcs
typecheckDataDeclaration md n (PrimitiveDataDeclaration prim) = return Map.empty

typecheckDataDeclarations :: ModuleName -> DataDeclarationMap CoreName -> TypecheckMonad (Map QCoreName (PolyType CoreName))
typecheckDataDeclarations md ds = do
  fmap Map.unions $ forM (Map.toList ds) $ \(k, v) -> typecheckDataDeclaration md (QName md TypeConstructorName k) v

typecheckModule :: Module CoreName -> TypecheckMonad (Map QCoreName (PolyType CoreName), C.Module)
typecheckModule (Module md is ds bs) = do
  env <- environmentTypeMap <$> ask
  traceShow env $ return ()
  ds' <- typecheckDataDeclarations md ds
  bs' <- globalBindMany ds' $ typecheckBindings md bs
  return (Map.union ds' $ Map.map fst . Map.mapKeys (QName md VariableName) $ bs', C.Module md bs')
