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

import Debug.Trace

type UnifyMap   = Map CoreName (MonoType CoreName)

data UnifyError = UnifyError (MonoType CoreName) (MonoType CoreName)
                | InfiniteType (MonoType CoreName) (MonoType CoreName)
                | InUnification (MonoType CoreName) (MonoType CoreName) UnifyError
                deriving (Show)

type UnifyMonad = StateT UnifyMap (Except UnifyError)

runUnifyMonad :: UnifyMonad a -> Either UnifyError a
runUnifyMonad = runExcept . flip evalStateT Map.empty

unifyType :: MonoType CoreName -> MonoType CoreName -> UnifyMonad ()
unifyType t1 t2 = unifyType' t1 t2 `catchError` (\e -> do
      env <- get
      traceShow env $ throwError $ InUnification t1 t2 e
    )
  where
    unifyType' t1@(TyConstant a1) t2@(TyConstant a2) | a1 == a2          = return ()
                                                    | otherwise         = throwError $ UnifyError t1 t2
    unifyType' (TyApplication a1 b1) (TyApplication a2 b2)               = do
      unifyType' a1 a2
      unifyType' b1 b2
    unifyType' t1@(TyApplication _ _) t2@(TyConstant _)                  = throwError $ UnifyError t1 t2
    unifyType' t1@(TyConstant _) t2@(TyApplication _ _)                  = throwError $ UnifyError t1 t2
    unifyType' t1@(TyApplication _ _) v1@(TyVariable _)                  = unifyType' v1 t1
    unifyType' t1@(TyConstant _) v1@(TyVariable _)                       = unifyType' v1 t1
    unifyType' (TyVariable v1) (TyVariable v2)               | v1 == v2  = return ()
    unifyType' (TyVariable v1) (TyVariable v2)               | otherwise = do
      x1 <- Map.lookup v1 <$> get
      x2 <- Map.lookup v2 <$> get
      case (x1, x2) of
        (Nothing, Nothing) -> modify $ Map.insert v1 (TyVariable v2)
        (Just x1, Nothing) -> unifyType' x1 (TyVariable v2)
        (Nothing, Just x2) -> unifyType' (TyVariable v1) x2
        (Just x1, Just x2) -> unifyType' x1 x2
    unifyType' (TyVariable v1) t1 | Set.member v1 (freeTypeVariables t1) = throwError $ InfiniteType (TyVariable v1) t1
    unifyType' (TyVariable v1) t1                                        = do
      x1 <- Map.lookup v1 <$> get
      case x1 of
        Nothing -> modify $ Map.insert v1 t1
        Just x1 -> unifyType' x1 t1

substituteType :: MonoType CoreName -> UnifyMonad (MonoType CoreName)
substituteType (TyConstant n)      = return $ TyConstant n
substituteType (TyApplication a b) = liftM2 TyApplication (substituteType a) (substituteType b)
substituteType (TyVariable v)      = do
  x <- Map.lookup v <$> get
  case x of
    Nothing -> return $ TyVariable v
    Just x  -> substituteType x

unifyMonoType :: MonoType CoreName -> MonoType CoreName -> UnifyMonad (MonoType CoreName)
unifyMonoType t1 t2 = do
  unifyType t1 t2
  substituteType t1

unifyPolyType :: PolyType CoreName -> PolyType CoreName -> UnifyMonad (PolyType CoreName)
unifyPolyType (PolyType vs1 t1) (PolyType vs2 t2) = do
  unifyType t1 t2
  monoType <- substituteType t1
  return $ PolyType (Set.intersection (Set.union vs1 vs2) (freeTypeVariables monoType)) monoType
