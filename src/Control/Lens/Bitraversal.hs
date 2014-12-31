{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module Control.Lens.Bitraversal where

import Data.Bitraversable

import Control.Lens
import Control.Applicative

type Bitraversal s t a b c d = forall f. Applicative f => (a -> f b) -> (c -> f d) -> (s -> f t)
