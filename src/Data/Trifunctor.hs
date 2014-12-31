{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Trifunctor where

import Data.Bifunctor

class Trifunctor f where
  trimap :: (a -> a') -> (b -> b') -> (c -> c') -> f a b c -> f a' b' c'

instance Trifunctor f => Bifunctor (f a) where
  bimap = trimap id
