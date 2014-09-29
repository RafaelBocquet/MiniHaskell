{-# LANGUAGE FlexibleContexts #-}

module Desugar.Typecheck where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Syntax.Expression
import Syntax.Module
import Syntax.Name
import Syntax.Location
import Desugar.Unify

type TypecheckMap   = Map NameId PolyType

data TypecheckError = UnboundVariable NameId
                    | TCUnifyError UnifyError
                    deriving (Show)

type TypecheckMonad = ReaderT TypecheckMap (ExceptT TypecheckError UnifyMonad)

baseTypecheckMap :: State NameId TypecheckMap
baseTypecheckMap = do
  a <- generateName
  b <- generateName
  c <- generateName
  d <- generateName
  e <- generateName
  f <- generateName
  return $ Map.fromList
    [ (UserName "[]", PolyType (Set.singleton f) (TyApplication (UserName "[]") [TyVariable f]))
    , (UserName ":", PolyType (Set.singleton a) $ 
        TyApplication (UserName "->")
          [ TyVariable a
          , TyApplication (UserName "->")
              [ TyApplication (UserName "[]") [TyVariable a]
              , TyApplication (UserName "[]") [TyVariable a]
              ]
          ]
      )
    , (UserName "return", PolyType (Set.singleton b) $
        TyApplication (UserName "->")
          [ TyVariable b
          , TyApplication (UserName "IO") [TyVariable b]
          ]
      )
    , (UserName ">>=", PolyType (Set.fromList [c, d]) $
        TyApplication (UserName "->")
          [ TyApplication (UserName "IO") [TyVariable c]
          , TyApplication (UserName "->")
            [ TyApplication (UserName "->")
              [TyVariable c
              , TyApplication (UserName "IO") [TyVariable d]
              ]
            , TyApplication (UserName "IO") [TyVariable d]
            ]
          ]
      )
    , (UserName "==", PolyType Set.empty $
        TyApplication (UserName "->")
          [ TyApplication (UserName "Integer") []
          , TyApplication (UserName "->")
              [ TyApplication (UserName "Integer") []
              , TyApplication (UserName "Integer") []
              ]
          ]
      )
    , (UserName "/=", PolyType Set.empty $
        TyApplication (UserName "->")
          [ TyApplication (UserName "Integer") []
          , TyApplication (UserName "->")
              [ TyApplication (UserName "Integer") []
              , TyApplication (UserName "Integer") []
              ]
          ]
      )
    , (UserName "<", PolyType Set.empty $
        TyApplication (UserName "->")
          [ TyApplication (UserName "Integer") []
          , TyApplication (UserName "->")
              [ TyApplication (UserName "Integer") []
              , TyApplication (UserName "Integer") []
              ]
          ]
      )
    , (UserName "<=", PolyType Set.empty $
        TyApplication (UserName "->")
          [ TyApplication (UserName "Integer") []
          , TyApplication (UserName "->")
              [ TyApplication (UserName "Integer") []
              , TyApplication (UserName "Integer") []
              ]
          ]
      )
    , (UserName ">", PolyType Set.empty $
        TyApplication (UserName "->")
          [ TyApplication (UserName "Integer") []
          , TyApplication (UserName "->")
              [ TyApplication (UserName "Integer") []
              , TyApplication (UserName "Integer") []
              ]
          ]
      )
    , (UserName ">=", PolyType Set.empty $
        TyApplication (UserName "->")
          [ TyApplication (UserName "Integer") []
          , TyApplication (UserName "->")
              [ TyApplication (UserName "Integer") []
              , TyApplication (UserName "Integer") []
              ]
          ]
      )
    , (UserName "+", PolyType Set.empty $
        TyApplication (UserName "->")
          [ TyApplication (UserName "Integer") []
          , TyApplication (UserName "->")
              [ TyApplication (UserName "Integer") []
              , TyApplication (UserName "Integer") []
              ]
          ]
      )
    , (UserName "-", PolyType Set.empty $
        TyApplication (UserName "->")
          [ TyApplication (UserName "Integer") []
          , TyApplication (UserName "->")
              [ TyApplication (UserName "Integer") []
              , TyApplication (UserName "Integer") []
              ]
          ]
      )
    , (UserName "/", PolyType Set.empty $
        TyApplication (UserName "->")
          [ TyApplication (UserName "Integer") []
          , TyApplication (UserName "->")
              [ TyApplication (UserName "Integer") []
              , TyApplication (UserName "Integer") []
              ]
          ]
      )
    , (UserName "*", PolyType Set.empty $
        TyApplication (UserName "->")
          [ TyApplication (UserName "Integer") []
          , TyApplication (UserName "->")
              [ TyApplication (UserName "Integer") []
              , TyApplication (UserName "Integer") []
              ]
          ]
      )
    , (UserName "fromInteger", PolyType Set.empty $
        TyApplication (UserName "->")
          [ TyApplication (UserName "Integer") []
          , TyApplication (UserName "Integer") []
          ]
      )
    , (UserName "()", PolyType Set.empty $
        TyApplication (UserName "()") []
      )
    , (UserName "error", PolyType (Set.singleton e) $
        TyApplication (UserName "->")
          [ TyApplication (UserName "[]") [TyApplication (UserName "Char") []]
          , TyVariable e
          ]
      )
    , (UserName "putChar", PolyType Set.empty $
        TyApplication (UserName "->")
          [ TyApplication (UserName "Char") []
          , TyApplication (UserName "IO") [TyApplication (UserName "()") []]
          ]
      )
    ]

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

environmentVariables :: TypecheckMap -> Set NameId
environmentVariables = Set.unions . (freePolyTypeVariables <$>) . Map.elems

typecheckExpression :: Expression -> TypecheckMonad MonoType
typecheckExpression e = typecheckExpression' (delocate e)
  where
    typecheckExpression' (EInteger _)                                 = return $ TyApplication (UserName "Integer") []
    typecheckExpression' (EChar _)                                    = return $ TyApplication (UserName "Char") []
    typecheckExpression' (EVariable (QName [] (Name _ x))) = do
      t <- Map.lookup x <$> ask
      case t of
        Nothing -> throwError $ UnboundVariable x
        Just t  -> instanciateType t
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
  let (xs, es) = (fmap fst *** fmap snd) $ Map.toList bs
  ts <- forM xs (const $ TyVariable <$> (lift.lift.lift $ generateName))
  local (Map.union $ Map.fromList $ zip xs (PolyType Set.empty <$> ts)) $ forM_ (zip es ts) $ \(e, t) -> do
    eTy <- typecheckExpression e
    lift.lift $ unifyType t eTy
  ts    <- lift.lift $ substituteType `mapM` ts
  freeG <- environmentVariables <$> ask
  let tvs = uncurry PolyType <$> zip (flip Set.difference freeG . freeTypeVariables <$> ts) ts
  return $ Map.fromList $ zip xs tvs

typecheckModule :: Module -> TypecheckMonad (Map NameId PolyType)
typecheckModule (Module _ bs) = typecheckBindings bs