{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Tritraversable where

import Data.Trifunctor
import Data.Trifoldable
import Data.Bitraversable

class (Trifunctor t, Trifoldable t) => Tritraversable t where
  tritraverse :: Applicative f => (a -> f a') -> (b -> f b') -> (c -> f c') -> t a b c -> f (t a' b' c')

instance Tritraversable f => Bitraversable (f a) where
  bitraverse = tritraverse pure
