{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Annotation where

import Control.Lens
import Control.Applicative
import Control.Monad

import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable

import Control.Monad.Trans
import Control.Monad.Morph

data Ann' e a a' = Ann
                   { annotation :: a
                   , unAnnotate :: e (Ann e a')
                   }
type Ann e a = Ann' e a a

-- Inductive annotations

type InductiveAnn e a b = Ann' e a (a, b) -> b

class InductiveAnnotation e a b where
  inductiveAnnotation :: InductiveAnn e a b

class InductiveAnnotable e a where
  inductiveAnnotateCls :: InductiveAnn e () a
instance InductiveAnnotable e () where
  inductiveAnnotateCls _ = ()
instance (InductiveAnnotable e a, InductiveAnnotation e a b) => InductiveAnnotable e (a, b) where
  inductiveAnnotateCls = undefined

inductiveExtend :: InductiveAnn e a b -> Ann' e a (a, b) -> Ann e (a, b)
inductiveExtend ind (Ann a x) = Ann (a, ind (Ann a x)) x

inductiveAnnotate :: Functor e => InductiveAnn e a b -> Ann e a -> Ann e (a, b)
inductiveAnnotate ind (Ann a x) =
  let x' = fmap (inductiveAnnotate ind) x in
  inductiveExtend ind (Ann a x')

ann :: InductiveAnnotable e a => e (Ann e ((), a)) -> Ann e ((), a)
ann x = inductiveExtend inductiveAnnotateCls (Ann () x)

-- Monadic inductive annotations

data Nat m = Nat { unNat :: forall i. m i -> m i }

type InductiveAnnM' e a b m = m ( e (Nat m, Ann e a)
                                , Ann' e a (a, b) -> m b
                                )
type InductiveAnnM e a b m = Ann e a -> InductiveAnnM' e a b m

simpleInductiveAnnM' :: (Functor e, Monad m) => Ann e a -> (Ann' e a (a, b) -> m b) -> InductiveAnnM' e a b m
simpleInductiveAnnM' = flip simpleInductiveAnnM

simpleInductiveAnnM :: (Functor e, Monad m) => (Ann' e a (a, b) -> m b) -> InductiveAnnM e a b m
simpleInductiveAnnM f (Ann _ e) = return ( fmap (Nat id,) e
                                         , f
                                         )
class InductiveAnnotationM e a b m where
  inductiveAnnotationM :: InductiveAnnM e a b m

-- Lifting non monadic instances
instance (Functor e, InductiveAnnotation e a b, Monad m) => InductiveAnnotationM e a b m where
  inductiveAnnotationM = simpleInductiveAnnM (return . inductiveAnnotation)

-- Lifting instances defined in smaller monads
instance (InductiveAnnotationM e a b m, Functor e, Monad m, Monad (t m), MonadTrans t, MFunctor t) => InductiveAnnotationM e a b (t m) where
  inductiveAnnotationM x = do
    lift (inductiveAnnotationM x)
    <&> bimap
    (fmap . first $ \(Nat a) -> Nat (hoist a)) -- using Nat . hoist won't work because of predicativity...
    (lift .)

class InductiveAnnotableM e a m where
  inductiveAnnotateClsM :: InductiveAnnM e () a m
instance (Functor e, Monad m) => InductiveAnnotableM e () m where
  inductiveAnnotateClsM = simpleInductiveAnnM (const (return ()))
instance (InductiveAnnotableM e a m, InductiveAnnotationM e a b m) => InductiveAnnotableM e (a, b) m where
  inductiveAnnotateClsM = undefined

inductiveExtendM :: Monad m => (Ann' e a (a, b) -> m b) -> Ann' e a (a, b) -> m (Ann e (a, b))
inductiveExtendM ind (Ann a x) = do
  b <- ind (Ann a x)
  return $ Ann (a, b) x

inductiveAnnotateM :: (Traversable e, Monad m) => InductiveAnnM e a b m -> Ann e a -> m (Ann e (a, b))
inductiveAnnotateM ind (Ann a x) = do
  (x', f) <- ind (Ann a x)
  x''     <- traverse (\(Nat n, y) -> n $ inductiveAnnotateM ind y) x'
  b       <- f (Ann a x'')
  return $ Ann (a, b) x''

