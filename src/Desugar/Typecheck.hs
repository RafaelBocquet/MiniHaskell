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

import qualified Core.Module as C
import qualified Core.Expression as C

import Debug.Trace

data TypecheckState = TypecheckState
  { typecheckMap    :: Map NameId PolyType
  , typecheckRename :: Map NameId NameId
  }

data TypecheckError = UnboundVariable NameId
                    | TCUnifyError UnifyError
                    deriving (Show)

type TypecheckMonad = ReaderT TypecheckState (ExceptT TypecheckError UnifyMonad)

addPrimitive :: String -> Int -> ([MonoType] -> MonoType) -> StateT TypecheckState (State NameId) ()
addPrimitive un nf f = do
  ns <- forM [1..nf] $ \_ -> lift generateName
  na <- generateName
  modify $ \s -> s { typecheckMap    = Map.insert na (PolyType (Set.fromList ns) (f $ TyVariable <$> ns)) (typecheckMap s)
                   , typecheckRename = Map.insert (UserName un) na (typecheckRename s)
                   }

baseTypecheckMap :: State NameId TypecheckState
baseTypecheckMap = flip execStateT (TypecheckState Map.empty Map.empty) $ do
  addPrimitive "[]"          1 $ \[a]    -> tyList a
  addPrimitive ":"           1 $ \[a]    -> tyArrowList [a, tyList a] (tyList a)
  addPrimitive "return"      1 $ \[a]    -> tyIO a
  addPrimitive ">>="         2 $ \[a, b] -> tyArrowList [tyIO a, tyArrow a (tyIO b)] (tyIO b)
  forM_ ["==", "/=", "<", "<=", ">", ">="] $ \na ->
    addPrimitive na          0 $ \[]      -> tyArrowList [tyInteger, tyInteger] tyBool
  forM_ ["+", "-", "*", "div", "rem"] $ \na ->
    addPrimitive na          0 $ \[]      -> tyArrowList [tyInteger, tyInteger] tyInteger
  forM_ ["True", "False"] $ \na ->
    addPrimitive na          0 $ \[]      -> tyBool
  forM_ ["&&", "||"] $ \na ->
    addPrimitive na          0 $ \[]      -> tyArrowList [tyBool, tyBool] tyBool
  addPrimitive "negate"      0 $ \[]      -> tyArrow tyInteger tyInteger
  addPrimitive "fromInteger" 0 $ \[]      -> tyArrow tyInteger tyInteger
  addPrimitive "()"          0 $ \[]      -> tyUnit
  addPrimitive "error"       1 $ \[a]     -> tyArrow (tyList tyChar) a
  addPrimitive "putChar"     0 $ \[]      -> tyIO tyUnit

runTypecheckMonad :: TypecheckMonad a -> State NameId (Either TypecheckError a)
runTypecheckMonad m = do
  tcEnv <- baseTypecheckMap
  v <- runUnifyMonad . runExceptT . flip runReaderT tcEnv $ m
  case v of
    Left e          -> return (Left $ TCUnifyError e)
    Right (Left e)  -> return (Left e)
    Right (Right r) -> return (Right r)

(***) f g x = (f x, g x)

instanciateType :: PolyType -> TypecheckMonad MonoType
instanciateType (PolyType as t) = do
    subst <- Map.fromList <$> forM (Set.toList as) (\a -> (,) a <$> (lift.lift.lift $ generateName))
    let applySubstitution (TyVariable a)       = case Map.lookup a subst of
          Just b  -> TyVariable b
          Nothing -> TyVariable a
        applySubstitution (TyApplication d ts) = TyApplication d (applySubstitution <$> ts)
    applySubstitution <$> (lift.lift $ substituteType t)

environmentVariables :: TypecheckState -> Set NameId
environmentVariables = Set.unions . (freePolyTypeVariables <$>) . Map.elems

typecheckExpression :: Expression -> TypecheckMonad C.Expression
typecheckExpression e = typecheckExpression' (delocate e)
  where
    typecheckExpression' (EInteger i)                                 = return $ C.Expression (TyApplication (UserName "Integer") []) (C.EInteger i)
    typecheckExpression' (EChar c)                                    = return $ C.Expression (TyApplication (UserName "Char") []) (C.EChar c)
    typecheckExpression' (EVariable (QName [] (Name _ x))) = do
      t <- Map.lookup x . typecheckMap <$> ask
      n <- Map.lookup x . typecheckRename <$> ask
      case t of
        Nothing -> throwError $ UnboundVariable x
        Just t  -> do
          ty <- instanciateType t
          case n of
            Nothing -> x
            Just n' -> n'
          return $ C.Expression ty ()
    typecheckExpression' (EApplication f t)                           = do
      fTy   <- typecheckExpression f
      tTy   <- typecheckExpression t
      tau   <- lift.lift.lift $ generateName
      sigma <- lift.lift.lift $ generateName
      lift.lift $ unifyType tTy (TyVariable tau)
      lift.lift $ unifyType fTy (TyApplication (UserName "->") [TyVariable tau, TyVariable sigma])
      lift.lift $ substituteType (TyVariable sigma)
    typecheckExpression' (ELambda x e)                                = do
      tau <- lift.lift.lift $ generateName
      eTy <- local (Map.insert x (PolyType Set.empty (TyVariable tau))) $ typecheckExpression e
      return $ TyApplication (UserName "->") [TyVariable tau, eTy]
    typecheckExpression' (ETuple es)                                  = do
      ts <- mapM typecheckExpression es
      return $ TyApplication (UserName $ replicate (length ts) ',') ts
    typecheckExpression' (EIf c a b)                                  = do
      cTy <- typecheckExpression c
      aTy <- typecheckExpression a
      bTy <- typecheckExpression b
      lift.lift $ unifyType cTy (TyApplication (UserName "Bool") [])
      lift.lift $ unifyMonoType aTy bTy
    typecheckExpression' (ELet bs e)                                  = do
      bts <- typecheckBindings bs
      local (Map.union bts) $ typecheckExpression e
    typecheckExpression' (EListCase e nil x xs r)                     = do
      eTy   <- typecheckExpression e
      tau   <- lift.lift.lift $ generateName
      lift.lift $ unifyType eTy (TyApplication (UserName "[]") [TyVariable tau])
      nilTy <- typecheckExpression nil
      rTy   <- local (Map.insert x (PolyType Set.empty (TyVariable tau)) . Map.insert xs (PolyType Set.empty eTy)) $ typecheckExpression r
      lift.lift $ unifyType nilTy rTy
      return nilTy

typecheckBindings :: BindingMap -> TypecheckMonad (Map NameId PolyType)
typecheckBindings bs = do
    let bgs = reverse $ topologicalSort (Set.fromList $ Map.keys bs) []
    foldlM (\bts bg -> (Map.union bts <$>) $ local (Map.union bts) $ typecheckBindings' bg) Map.empty bgs
  where
    dependenciesMapStep :: Map NameId (Set NameId) -> Map NameId (Set NameId)
    dependenciesMapStep mp = Map.mapWithKey (\k v -> Set.union v $ Set.unions $ fromJust . flip Map.lookup mp <$> Set.toList v) mp

    makeDependenciesMap :: Int -> Map NameId (Set NameId) -> Map NameId (Set NameId)
    makeDependenciesMap 0 = id
    makeDependenciesMap n = makeDependenciesMap (n `div` 2) . dependenciesMapStep

    dependenciesMap :: Map NameId (Set NameId)
    dependenciesMap = makeDependenciesMap (Map.size bs) (Map.map (Set.intersection (Set.fromList $ Map.keys bs) . expressionFreeVariables) bs)

    reverseDependenciesMap :: Map NameId (Set NameId)
    reverseDependenciesMap = Map.foldWithKey (\k -> flip . Set.fold . Map.update $ Just . Set.insert k) (Map.map (const Set.empty) bs) dependenciesMap

    topologicalSort :: Set NameId -> [Set NameId] -> [Set NameId]
    topologicalSort st acc
      | Set.null st = acc
      | otherwise   =
          let elem       = Set.findMin st
              allElemDep = fromJust $ Map.lookup elem dependenciesMap
              revElemDep = fromJust $ Map.lookup elem reverseDependenciesMap
              elemSet    = Set.insert elem $ Set.intersection allElemDep revElemDep
              elemDep    = allElemDep `Set.difference` elemSet
              acc' = topologicalSort elemDep acc
            in
          topologicalSort (Set.delete elem $ st `Set.difference` allElemDep) (elemSet : acc')

    typecheckBindings' :: Set NameId -> TypecheckMonad (Map NameId PolyType)
    typecheckBindings' st = do
      let xs = Set.toList st
          es = fromJust . flip Map.lookup bs <$> xs
      ts <- forM xs (const $ TyVariable <$> (lift.lift.lift $ generateName))
      local (Map.union $ Map.fromList $ zip xs (PolyType Set.empty <$> ts)) $ forM_ (zip es ts) $ \(e, t) -> do
        eTy <- typecheckExpression e
        lift.lift $ unifyType t eTy
      unMap <- lift.lift $ get
      ts    <- lift.lift $ substituteType `mapM` ts
      freeG <- environmentVariables <$> ask
      let tvs = uncurry PolyType <$> zip (flip Set.difference freeG . freeTypeVariables <$> ts) ts
      return $ Map.fromList $ zip xs tvs


typecheckModule :: Module -> TypecheckMonad C.Module
typecheckModule (Module _ bs) = typecheckBindings bs