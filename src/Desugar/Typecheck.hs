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

type TypecheckState = Map NameId PolyType

data TypecheckError = UnboundVariable NameId
                    | TCUnifyError UnifyError
                    deriving (Show)

type TypecheckMonad = ReaderT TypecheckState (ExceptT TypecheckError UnifyMonad)

addPrimitive :: String -> Int -> ([MonoType] -> MonoType) -> StateT TypecheckState (State NameId) ()
addPrimitive un nf f = do
  ns <- forM [1..nf] $ \_ -> lift generateName
  GenName na <- lift generateName
  modify $ \s -> s { typecheckMap    = Map.insert (GenName na) (PolyType (Set.fromList ns) (f $ TyVariable <$> ns)) (typecheckMap s)
                   , typecheckRename = Map.insert (UserName un) na (typecheckRename s)
                   }

baseTypecheckMap :: State NameId TypecheckState
baseTypecheckMap = flip execStateT (TypecheckState Map.empty Map.empty) $ do
  addPrimitive "[]"          1 $ \[a]    -> tyList a
  addPrimitive ":"           1 $ \[a]    -> tyArrowList [a, tyList a] (tyList a)
  addPrimitive "return"      1 $ \[a]    -> tyArrow a (tyIO a)
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
environmentVariables = Set.unions . (freePolyTypeVariables <$>) . Map.elems . typecheckMap

typecheckExpression :: Expression -> TypecheckMonad C.Expression
typecheckExpression e = typecheckExpression' (delocate e)
  where
    typecheckExpression' (EInteger i)                                 = return $ C.Expression (TyApplication (UserName "Integer") []) (C.EInteger i)
    typecheckExpression' (EChar c)                                    = return $ C.Expression (TyApplication (UserName "Char") []) (C.EChar c)
    typecheckExpression' (EVariable (QName [] (Name ns x))) = do
      n <- Map.lookup x . typecheckRename <$> ask
      case n of
        Just x -> typecheckExpression' (EVariable (QName [] (Name ns (GenName x))))
        Nothing -> do
          t <- Map.lookup x . typecheckMap <$> ask
          case t of
            Nothing -> throwError $ UnboundVariable x
            Just t  -> do
              ty <- instanciateType t
              return $ C.Expression ty (C.EVariable $ fromGenName x)
    typecheckExpression' (EApplication f t)                           = do
      fTy   <- typecheckExpression f
      tTy   <- typecheckExpression t
      tau   <- lift.lift.lift $ generateName
      sigma <- lift.lift.lift $ generateName
      lift.lift $ unifyType (C.expressionType tTy) (TyVariable tau)
      lift.lift $ unifyType (C.expressionType fTy) (tyArrow (TyVariable tau) (TyVariable sigma))
      return $ C.Expression (TyVariable sigma) (C.EApplication fTy tTy)
    typecheckExpression' (ELambda x e)                                = do
      tau <- lift.lift.lift $ generateName
      GenName xn <- lift.lift.lift $ generateName
      eTy <- local (\s -> s { typecheckMap    = Map.insert x (PolyType Set.empty (TyVariable tau)) $ typecheckMap s
                            , typecheckRename = Map.insert x xn $ typecheckRename s
                            }) $ typecheckExpression e
      return $ C.Expression (tyArrow (TyVariable tau) (C.expressionType eTy)) (C.ELambda xn eTy)
    typecheckExpression' (ETuple es)                                  = do
      ts <- mapM typecheckExpression es
      return $ C.Expression (tyTuple $ C.expressionType <$> ts) (C.ETuple ts)
    typecheckExpression' (EIf c a b)                                  = do
      cTy <- typecheckExpression c
      aTy <- typecheckExpression a
      bTy <- typecheckExpression b
      lift.lift $ unifyType (C.expressionType cTy) tyBool
      lift.lift $ unifyMonoType (C.expressionType aTy) (C.expressionType bTy)
      return $ C.Expression (C.expressionType aTy) (C.EIf cTy aTy bTy)
    typecheckExpression' (ELet bs e)                                  = do
      bts <- typecheckBindings bs
      eTy <- localBind bts $ typecheckExpression e
      return $ C.Expression (C.expressionType eTy) (C.ELet bts eTy)
    typecheckExpression' (EListCase e nil x xs r)                     = do
      eTy   <- typecheckExpression e
      tau   <- lift.lift.lift $ generateName
      lift.lift $ unifyType (C.expressionType eTy) (tyList (TyVariable tau))
      nilTy <- typecheckExpression nil
      GenName xn  <- lift.lift.lift $ generateName
      GenName xsn <- lift.lift.lift $ generateName
      rTy   <- local (\s -> s { typecheckMap    = Map.insert x  (PolyType Set.empty (TyVariable tau))
                                                . Map.insert xs (PolyType Set.empty (C.expressionType eTy))
                                                $ typecheckMap s
                              , typecheckRename = Map.insert x xn
                                                . Map.insert xs xsn
                                                $ typecheckRename s
                              }) $ typecheckExpression r
      lift.lift $ unifyType (C.expressionType nilTy) (C.expressionType rTy)
      return $ C.Expression (C.expressionType nilTy) (C.EListCase eTy nilTy xn xsn rTy)

localBind :: C.BindingMap -> TypecheckMonad a -> TypecheckMonad a
localBind bindings = local (\s -> s { typecheckMap = Map.union (Map.map fst . Map.mapKeys GenName $ bindings) (typecheckMap s) })

typecheckBindings :: BindingMap -> TypecheckMonad C.BindingMap
typecheckBindings bs = do
    let bgs = reverse $ topologicalSort (Set.fromList $ Map.keys bs) []
    foldlM
      (\bts bg -> (Map.union bts <$>) $ localBind bts $ (Map.union bts) <$> typecheckBindings' bg)
      Map.empty
      bgs
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

    typecheckBindings' :: Set NameId -> TypecheckMonad C.BindingMap
    typecheckBindings' st = do
      let xs = Set.toList st
          es = fromJust . flip Map.lookup bs <$> xs
      ts   <- forM xs (const $ TyVariable <$> (lift.lift.lift $ generateName))
      xsn  <- forM xs (const $ lift.lift.lift $ fromGenName <$> generateName)
      esTy <- local (\s -> s { typecheckMap    = Map.union (Map.fromList $ zip xs (PolyType Set.empty <$> ts)) $ typecheckMap s
                             , typecheckRename = Map.union (Map.fromList $ zip xs xsn)                         $ typecheckRename s
                             }) $ forM (zip es ts) $ \(e, t) -> do
        eTy <- typecheckExpression e
        lift.lift $ unifyType t (C.expressionType eTy)
        return $ eTy
      unMap <- lift.lift $ get
      ts    <- lift.lift $ substituteType `mapM` ts
      freeG <- environmentVariables <$> ask
      let tvs = uncurry PolyType <$> zip (flip Set.difference freeG . freeTypeVariables <$> ts) ts
      return $ Map.fromList $ zip xsn (zip tvs esTy)


typecheckModule :: Module -> TypecheckMonad C.Module
typecheckModule (Module n bs) = C.Module n <$> typecheckBindings bs