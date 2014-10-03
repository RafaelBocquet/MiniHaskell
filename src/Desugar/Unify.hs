{-# LANGUAGE FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Desugar.Unify where

import Control.Monad.Except
import Control.Monad.State
import Control.Applicative

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Syntax.Expression
import Syntax.Type
import Syntax.Name

type UnifyMap   = Map CoreName (MonoType CoreName)

data UnifyError = UnifyError (MonoType CoreName) (MonoType CoreName)
                | InfiniteType (MonoType CoreName) (MonoType CoreName)
                deriving (Show)

type UnifyMonad = StateT UnifyMap (ExceptT UnifyError (State CoreName))

runUnifyMonad :: UnifyMonad a -> State CoreName (Either UnifyError a)
runUnifyMonad = runExceptT . flip evalStateT Map.empty

class (Applicative m, MonadState UnifyMap m, MonadError UnifyError m) => MonadUnify m where
instance (Applicative m, MonadState UnifyMap m, MonadError UnifyError m) => MonadUnify m where

unifyType :: MonadUnify m => (MonoType CoreName) -> (MonoType CoreName) -> m ()
unifyType t1@(TyApplication d1 as1) t2@(TyApplication d2 as2) | d1 /= d2                 = throwError $ UnifyError t1 t2
unifyType t1@(TyApplication d1 as1) t2@(TyApplication d2 as2) | length as1 /= length as2 = throwError $ UnifyError t1 t2
unifyType (TyApplication d1 as1) (TyApplication d2 as2) | otherwise                      = forM_ (zip as1 as2) (uncurry unifyType)
unifyType t1@(TyApplication _ _) v1@(TyVariable _)                                       = unifyType v1 t1
unifyType (TyVariable v1) t1@(TyApplication _ _) | Set.member v1 (freeTypeVariables t1)  = throwError $ InfiniteType (TyVariable v1) t1
unifyType (TyVariable v1) t1@(TyApplication _ _)                                         = do
  x1 <- Map.lookup v1 <$> get
  case x1 of
    Nothing -> modify $ Map.insert v1 t1
    Just x1 -> unifyType x1 t1
unifyType (TyVariable v1) (TyVariable v2)               | v1 == v2                       = return ()
unifyType (TyVariable v1) (TyVariable v2)               | otherwise                      = do
  x1 <- Map.lookup v1 <$> get
  x2 <- Map.lookup v2 <$> get
  case (x1, x2) of
    (Nothing, Nothing) -> modify $ Map.insert v1 (TyVariable v2)
    (Just x1, Nothing) -> unifyType x1 (TyVariable v2)
    (Nothing, Just x2) -> unifyType (TyVariable v1) x2
    (Just x1, Just x2) -> unifyType x1 x2

substituteType :: MonadUnify m => (MonoType CoreName) -> m (MonoType CoreName)
substituteType (TyApplication d as) = TyApplication d <$> mapM substituteType as
substituteType (TyVariable v)       = do
  x <- Map.lookup v <$> get
  case x of
    Nothing -> return $ TyVariable v
    Just x  -> substituteType x

unifyMonoType :: MonadUnify m => (MonoType CoreName) -> (MonoType CoreName) -> m (MonoType CoreName)
unifyMonoType t1 t2 = do
  unifyType t1 t2
  substituteType t1

unifyPolyType :: MonadUnify m => (PolyType CoreName) -> (PolyType CoreName) -> m (PolyType CoreName)
unifyPolyType (PolyType vs1 t1) (PolyType vs2 t2) = do
  unifyType t1 t2
  monoType <- substituteType t1
  return $ PolyType (Set.intersection (Set.union vs1 vs2) (freeTypeVariables monoType)) monoType
