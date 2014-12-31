{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Trifoldable where

import Data.Monoid
import Data.Trifunctor
import Data.Bifoldable

class Trifunctor t => Trifoldable t where
  trifoldMap :: Monoid m => (a -> m) -> (b -> m) -> (c -> m) -> t a b c -> m

instance Trifoldable f => Bifoldable (f a) where
  bifoldMap = trifoldMap (const mempty)
