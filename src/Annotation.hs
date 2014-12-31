{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections #-}

module Annotation where

import Control.Lens
import Control.Applicative
import Control.Monad

import Data.Bifoldable
import Data.Bitraversable

data Ann' e a a' = Ann
                   { annotation :: a
                   , unAnnotate :: e (Ann e a')
                   }
type Ann e a = Ann' e a a

fixMap :: Bifunctor e => (n -> n') -> Ann' (e n) a a' -> Ann' (e n') a a'
fixMap f (Ann a e) = Ann a $ bimap f (fixMap f) e

fixFoldMap :: (Bifoldable e, Monoid m) => (n -> m) -> Ann' (e n) a a' -> m
fixFoldMap f (Ann a e) = bifoldMap f (fixFoldMap f) e

fixTraverse :: (Bitraversable e, Applicative f) => (n -> f n') -> Ann' (e n) a a' -> f (Ann' (e n') a a')
fixTraverse f (Ann a e) = Ann a <$> bitraverse f (fixTraverse f) e

ann :: e (Ann e a) -> Ann' e () a
ann = Ann ()

type InductiveAnn' e a b m = m ( e (m (Ann e (a, b)) -> m (Ann e (a, b)), Ann e a)
                               , Ann' e a (a, b) -> m b
                               )
type InductiveAnn e a b m = Ann e a -> InductiveAnn' e a b m

simpleInductiveAnn' :: (Functor e, Monad m) => Ann e a -> (Ann' e a (a, b) -> m b) -> InductiveAnn' e a b m
simpleInductiveAnn' = flip simpleInductiveAnn

simpleInductiveAnn :: (Functor e, Monad m) => (Ann' e a (a, b) -> m b) -> InductiveAnn e a b m
simpleInductiveAnn f (Ann _ e) = return ( fmap (id,) e
                                        , f
                                        )

inductiveAnnotate:: (Traversable e, Applicative m, Monad m) => InductiveAnn e a b m -> Ann e a -> m (Ann e (a, b))
inductiveAnnotate ind x@(Ann a _) = do
  (e, f) <- ind x
  e' <- e
       & traverse
       (\(mt, st) -> mt $ inductiveAnnotate ind st)
  b <- f $ Ann a e'
  return $ Ann (a, b) e'

