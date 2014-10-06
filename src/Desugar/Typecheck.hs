{-# LANGUAGE FlexibleContexts #-}

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
  { environmentMap      :: Map (QName CoreName) (PolyType CoreName)
  , environmentRenaming :: RenameMap
  }

data TypecheckError = UnboundVariable (QName CoreName)
                    | TCUnifyError UnifyError
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

localBind :: CoreName -> PolyType CoreName -> TypecheckMonad a -> TypecheckMonad a
localBind x t = local $ \s -> s { environmentMap = Map.insert (QName [] (Name VariableName x)) t (environmentMap s) }

localBindMany :: Map CoreName (PolyType CoreName) -> TypecheckMonad a -> TypecheckMonad a
localBindMany bs = local $ \s -> s { environmentMap = Map.union (Map.mapKeys (QName [] . Name VariableName) bs) (environmentMap s) }

getPrimitive :: NameSpace -> String -> TypecheckMonad (QName CoreName)
getPrimitive ns s = do
  p <- Map.lookup (QName ["Primitive"] (Name ns (UserName s))) . environmentRenaming <$> ask
  case p of
    Just (RenameGlobal [p]) -> return p
    _                       -> throwError $ UnknownPrimitive s

instanciateType :: PolyType CoreName -> TypecheckMonad (MonoType CoreName)
instanciateType (PolyType as t) = do
    subst <- Map.fromList <$> forM (Set.toList as) (\a -> (,) a <$> generateName)
    let applySubstitution (TyVariable a)       = case Map.lookup a subst of
          Just b  -> TyVariable b
          Nothing -> TyVariable a
        applySubstitution (TyApplication d ts) = TyApplication d (applySubstitution <$> ts)
    applySubstitution <$> (lift.lift.lift $ substituteType t)

environmentVariables :: TypecheckMonad (Set CoreName)
environmentVariables = Set.unions . (freePolyTypeVariables <$>) . Map.elems . environmentMap <$> ask

typecheckExpression :: Expression CoreName -> TypecheckMonad C.Expression
typecheckExpression e = typecheckExpression' (delocate e)
  where
    typecheckExpression' (EInteger i)                                 = do
      tyInt <- getPrimitive TypeConstructorName "Int"
      return $ C.Expression (TyApplication tyInt []) (C.EInteger i)
    typecheckExpression' (EChar c)                                    = do
      tyChar <- getPrimitive TypeConstructorName "Char"
      return $ C.Expression (TyApplication tyChar []) (C.EChar c)
    typecheckExpression' (EVariable x) = do
      t <- Map.lookup x . environmentMap <$> ask
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
      lift.lift.lift $ unifyType (C.expressionType tTy) (TyVariable tau)
      tyArrow <- TyApplication <$> getPrimitive TypeConstructorName "->"
      lift.lift.lift $ unifyType (C.expressionType fTy) (tyArrow [TyVariable tau, TyVariable sigma])
      return $ C.Expression (TyVariable sigma) (C.EApplication fTy tTy)
    typecheckExpression' (ELambda x e)                                = do
      tau     <- generateName
      tyArrow <- TyApplication <$> getPrimitive TypeConstructorName "->"
      eTy     <- localBind x (PolyType Set.empty (TyVariable tau)) $ typecheckExpression e
      return $ C.Expression (tyArrow [TyVariable tau, C.expressionType eTy]) (C.ELambda x eTy)
    typecheckExpression' (ETuple es)                                  = do
      ts <- mapM typecheckExpression es
      tyTuple <- getPrimitive TypeConstructorName (replicate (length es - 1) ',')
      return $ C.Expression (TyApplication tyTuple (C.expressionType <$> ts)) (C.ETuple ts)
    typecheckExpression' (EIf c a b)                                  = do
      cTy <- typecheckExpression c
      aTy <- typecheckExpression a
      bTy <- typecheckExpression b
      tyBool <- getPrimitive TypeConstructorName "Bool"
      lift.lift.lift $ unifyType (C.expressionType cTy) (TyApplication tyBool [])
      lift.lift.lift $ unifyMonoType (C.expressionType aTy) (C.expressionType bTy)
      return $ C.Expression (C.expressionType aTy) (C.EIf cTy aTy bTy)
    typecheckExpression' (ELet bs e)                                  = do
      bts <- typecheckBindings bs
      eTy <- localBindMany (Map.map fst bts) $ typecheckExpression e
      return $ C.Expression (C.expressionType eTy) (C.ELet bts eTy)
    typecheckExpression' (EListCase e nil x xs r)                     = do
      eTy    <- typecheckExpression e
      tau    <- generateName
      tyList <- getPrimitive TypeConstructorName "[]"
      lift.lift.lift $ unifyType (C.expressionType eTy) (TyApplication tyList [TyVariable tau])
      nilTy  <- typecheckExpression nil
      rTy    <- localBind x (PolyType Set.empty $ TyVariable tau)
              $ localBind xs (PolyType Set.empty $ TyApplication tyList [TyVariable tau])
              $ typecheckExpression r
      lift.lift.lift $ unifyType (C.expressionType nilTy) (C.expressionType rTy)
      return $ C.Expression (C.expressionType nilTy) (C.EListCase eTy nilTy x xs rTy)
    -- typecheckExpression' (PrimitiveDeclaration prim)                   = 

typecheckDeclaration :: Declaration CoreName -> TypecheckMonad C.Declaration
typecheckDeclaration (Declaration e)             = C.Declaration <$> typecheckExpression e
typecheckDeclaration (PrimitiveDeclaration prim) = return $ C.PrimitiveDeclaration prim

primitiveType :: PrimitiveDeclaration -> TypecheckMonad (MonoType CoreName)
primitiveType p 
  | p `elem` [FromIntegerDeclaration, NegateDeclaration]
      = do
    tyInt   <- flip TyApplication [] <$> getPrimitive TypeConstructorName "Int"
    tyArrow <- TyApplication <$> getPrimitive TypeConstructorName "->"
    return $ tyArrow [tyInt, tyInt]
  | p `elem` [ AddDeclaration, SubDeclaration, MulDeclaration, DivDeclaration, RemDeclaration
             , LTDeclaration, LEDeclaration, GTDeclaration, GEDeclaration, EQDeclaration, NEDeclaration]
      = do
    tyInt   <- flip TyApplication [] <$> getPrimitive TypeConstructorName "Int"
    tyArrow <- TyApplication <$> getPrimitive TypeConstructorName "->"
    return $ tyArrow [tyInt, tyArrow [tyInt, tyInt]]
  | p `elem` [ AndDeclaration, OrDeclaration]
      = do
    tyBool  <- flip TyApplication [] <$> getPrimitive TypeConstructorName "Bool"
    tyArrow <- TyApplication <$> getPrimitive TypeConstructorName "->"
    return $ tyArrow [tyBool, tyArrow [tyBool, tyBool]]
  | p == BindDeclaration
      = do
    a       <- TyVariable <$> generateName
    b       <- TyVariable <$> generateName
    tyIO    <- TyApplication <$> getPrimitive TypeConstructorName "IO"
    tyArrow <- TyApplication <$> getPrimitive TypeConstructorName "->"
    return $ tyArrow [tyIO [a], tyArrow [tyArrow [a, tyIO [b]], tyIO [b]]]
  | p == ReturnDeclaration
      = do
    a       <- TyVariable <$> generateName 
    tyIO    <- TyApplication <$> getPrimitive TypeConstructorName "IO"
    tyArrow <- TyApplication <$> getPrimitive TypeConstructorName "->"
    return $ tyArrow [a, tyIO [a]]

typecheckBindings :: DeclarationMap CoreName -> TypecheckMonad C.DeclarationMap
typecheckBindings bs = do
    let bgs = reverse $ topologicalSort (Set.fromList $ Map.keys bs) []
    foldlM
      (\bts bg -> (Map.union bts <$>) $ localBindMany (Map.map fst bts) $ (Map.union bts) <$> typecheckBindings' bg)
      Map.empty
      bgs
  where
    dependenciesMapStep :: Map CoreName (Set CoreName) -> Map CoreName (Set CoreName)
    dependenciesMapStep mp = Map.mapWithKey (\k v -> Set.union v $ Set.unions $ fromJust . flip Map.lookup mp <$> Set.toList v) mp

    makeDependenciesMap :: Int -> Map CoreName (Set CoreName) -> Map CoreName (Set CoreName)
    makeDependenciesMap 0 = id
    makeDependenciesMap n = makeDependenciesMap (n `div` 2) . dependenciesMapStep

    dependenciesMap :: Map CoreName (Set CoreName)
    dependenciesMap = makeDependenciesMap (Map.size bs) (Map.map (Set.intersection (Set.fromList $ Map.keys bs) . declarationFreeVariables) bs)

    reverseDependenciesMap :: Map CoreName (Set CoreName)
    reverseDependenciesMap = Map.foldWithKey (\k -> flip . Set.fold . Map.update $ Just . Set.insert k) (Map.map (const Set.empty) bs) dependenciesMap

    topologicalSort :: Set CoreName -> [Set CoreName] -> [Set CoreName]
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

    typecheckBindings' :: Set CoreName -> TypecheckMonad C.DeclarationMap
    typecheckBindings' st = do
      let xs = Set.toList st
          es = fromJust . flip Map.lookup bs <$> xs
      ts <- forM xs (const $ TyVariable <$> generateName)
      esTy <- localBindMany (Map.fromList $ zip xs (PolyType Set.empty <$> ts)) $ forM (zip es ts) $ \(e, t) -> do
        eTy <- typecheckDeclaration e
        case eTy of
          C.Declaration e             -> lift.lift.lift $ unifyType t (C.expressionType e)
          C.PrimitiveDeclaration prim -> do
            pTy <- primitiveType prim
            lift.lift.lift $ unifyType t pTy
        return $ eTy
      ts    <- lift.lift.lift $ substituteType `mapM` ts
      freeG <- environmentVariables
      let tvs = uncurry PolyType <$> zip ((`Set.difference` freeG) . freeTypeVariables <$> ts) ts
      return $ Map.fromList $ zip xs (zip tvs esTy)


typecheckModule :: Module CoreName -> TypecheckMonad C.Module
typecheckModule (Module n is ds bs) = C.Module n <$> typecheckBindings bs
