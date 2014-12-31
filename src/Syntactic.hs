{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Syntactic where

import Annotation

import Data.List
import Data.Maybe

import Data.Foldable
import Data.Traversable
import Data.Bifunctor
import Data.Bitraversable

import Control.Lens
import Control.Applicative
import Control.Monad.State hiding (forM)
import Control.Monad.Reader hiding (forM)

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

class Syntactic e where
  syntacticVariables :: Ord n => e n a -> Set n
  syntacticBinders   :: Ord n => e n a -> (Set n, e n (Set n, a))

instance (Syntactic e, Functor (e n), Foldable (e n), Ord n) => InductiveAnnotation (e n) a (Set n) where
  inductiveAnnotation (Ann _ e) = Set.union
                                  (syntacticVariables e)
                                  (foldMap
                                   (\(bound, Ann (_, subs) _) -> subs `Set.difference` bound)
                                   (snd (syntacticBinders e))
                                  )
