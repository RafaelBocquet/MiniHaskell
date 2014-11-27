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

data UnifyMap  = UnifyMap
  { unifyTypeMap       :: Map CoreName (MonoType CoreName)
  , unifyConstraintMap :: Map CoreName (Set QCoreName)
  , unifyKindMap       :: Map CoreName (Kind CoreName)
  }

data UnifyError = UnifyError (MonoType CoreName) (MonoType CoreName)
                | InfiniteType CoreName (MonoType CoreName)
                | InUnification (MonoType CoreName) (MonoType CoreName) UnifyError
                | UnifyKindError (Kind CoreName) (Kind CoreName)
                | InfiniteKind CoreName (Kind CoreName)
                | InKindUnification (Kind CoreName) (Kind CoreName) UnifyError

instance Show UnifyError where
  show (InUnification t1 t2 err)     = "Can't unify type\n\t" ++ show t1 ++ "\n with\n" ++ show t2 ++ "\n : \n\t" ++ show err
  show (InKindUnification k1 k2 err) = "Can't unify kind\n\t" ++ show k1 ++ "\n with\n" ++ show k2 ++ "\n : \n\t" ++ show err
  show (UnifyError t1 t2)            = "Can't unify type\n\t" ++ show t1 ++ "\n with\n" ++ show t2
  show (InfiniteType v t)            = "Type " ++ show t ++ " is infinite, because of type variable " ++ show v
  show (UnifyKindError k1 k2)        = "Can't unify kind\n\t" ++ show k1 ++ "\n with\n" ++ show k2
  show (InfiniteKind v k)            = "Kind " ++ show k ++ " is infinite, because of kind variable " ++ show v


type UnifyMonad = StateT UnifyMap (Except UnifyError)

runUnifyMonad :: UnifyMonad a -> Either UnifyError a
runUnifyMonad = runExcept . flip evalStateT (UnifyMap Map.empty Map.empty Map.empty)

unifyKind :: Kind CoreName -> Kind CoreName -> UnifyMonad ()
unifyKind k1 k2 = unifyKind' k1 k2 `catchError` (\e -> throwError $ InKindUnification k1 k2 e)
  where
    unifyKind' KStar KStar                                               = return ()
    unifyKind' (KArrow a1 b1) (KArrow a2 b2)                             = do
      unifyKind a1 a2
      unifyKind b1 b2
    unifyKind' (KVariable v1) (KVariable v2) | v1 == v2                  = return ()
                                            | otherwise                 = do
      x1 <- Map.lookup v1 . unifyKindMap <$> get
      x2 <- Map.lookup v2 . unifyKindMap <$> get
      case (x1, x2) of
        (Nothing, Nothing) -> modify $ \s -> s { unifyKindMap = Map.insert v1 (KVariable v2) (unifyKindMap s) }
        (Just x1, Nothing) -> unifyKind x1 (KVariable v2)
        (Nothing, Just x2) -> unifyKind (KVariable v1) x2
        (Just x1, Just x2) -> unifyKind x1 x2
    unifyKind' (KVariable v1) k1 | Set.member v1 (freeKindVariables k1) = throwError $ InfiniteKind v1 k1
                                 | otherwise                            = do
      x1 <- Map.lookup v1 . unifyKindMap <$> get
      case x1 of
        Nothing -> modify $ \s -> s { unifyKindMap = Map.insert v1 k1 (unifyKindMap s) }
        Just x1 -> unifyKind x1 k1
    unifyKind' k1 (KVariable v2)                                         = unifyKind (KVariable v2) k1
    unifyKind' k1 k2                                                     = throwError $ UnifyKindError k1 k2

unifyType :: MonoType CoreName -> MonoType CoreName -> UnifyMonad ()
unifyType t1 t2 = unifyType' t1 t2 `catchError` (\e -> throwError $ InUnification t1 t2 e)
  where
    unifyType' t1@(TyConstant a1) t2@(TyConstant a2) | a1 == a2           = return ()
    unifyType' TyArrow TyArrow                                           = return ()
    unifyType' (TyApplication a1 b1) (TyApplication a2 b2)               = do
      unifyType a1 a2
      unifyType b1 b2
    unifyType' (TyVariable v1) (TyVariable v2) | v1 == v2                 = return ()
                                               | otherwise               = do
      c1 <- Map.lookup v1 . unifyConstraintMap <$> get
      c2 <- Map.lookup v2 . unifyConstraintMap <$> get
      case (c1, c2) of
        (Nothing, Nothing) -> return ()
        (Just c1, Nothing) -> addConstraints v2 c1
        (Nothing, Just c2) -> addConstraints v1 c2
        (Just c1, Just c2) -> do
          addConstraints v1 c2
          addConstraints v2 c1
      x1 <- Map.lookup v1 . unifyTypeMap <$> get
      x2 <- Map.lookup v2 . unifyTypeMap <$> get
      case (x1, x2) of
        (Nothing, Nothing) -> modify $ \s -> s { unifyTypeMap              = Map.insert v1 (TyVariable v2) (unifyTypeMap s) }
        (Just x1, Nothing) -> unifyType x1 (TyVariable v2)
        (Nothing, Just x2) -> unifyType (TyVariable v1) x2
        (Just x1, Just x2) -> unifyType x1 x2
    unifyType' (TyVariable v1) t1 | Set.member v1 (freeTypeVariables t1) = throwError $ InfiniteType v1 t1
                                  | otherwise                            = do
      x1 <- Map.lookup v1 . unifyTypeMap <$> get
      case x1 of
        Nothing            -> modify $ \s -> s { unifyTypeMap              = Map.insert v1 t1 (unifyTypeMap s) }
                                             -- Add Constraints to t1...
        Just x1            -> unifyType x1 t1
    unifyType' t1 v1@(TyVariable _)                                      = unifyType v1 t1
    unifyType' t1 t2                                                     = throwError $ UnifyError t1 t2

addConstraints :: CoreName -> Set QCoreName -> UnifyMonad ()
addConstraints v c = modify $ \s -> s { unifyConstraintMap = Map.insertWith Set.union v c (unifyConstraintMap s) }

substituteType :: MonoType CoreName -> UnifyMonad (MonoType CoreName)
substituteType TyArrow             = return TyArrow
substituteType (TyConstant n)      = return $ TyConstant n
substituteType (TyApplication a b) = liftM2 TyApplication (substituteType a) (substituteType b)
substituteType (TyVariable v)      = do
  x <- Map.lookup v . unifyTypeMap <$> get
  case x of
    Nothing -> return $ TyVariable v
    Just x  -> do
      x' <- substituteType x
      modify $ \s -> s { unifyTypeMap = Map.insert v x' (unifyTypeMap s) }
      return x'

unifyMonoType :: MonoType CoreName -> MonoType CoreName -> UnifyMonad (MonoType CoreName)
unifyMonoType t1 t2 = do
  unifyType t1 t2
  substituteType t1

unifyPolyType :: PolyType CoreName -> PolyType CoreName -> UnifyMonad (PolyType CoreName)
unifyPolyType (PolyType vs1 t1) (PolyType vs2 t2) = do
  unifyType t1 t2
  monoType <- substituteType t1
  return $ PolyType (Map.filterWithKey (flip . const $ flip Set.member (freeTypeVariables monoType)) $ Map.intersectionWith Set.intersection vs1 vs2) monoType
