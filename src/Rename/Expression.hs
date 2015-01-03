{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections #-}
{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}

module Rename.Expression where

import Syntax.Expression
import Syntax.Name
import Syntax.Type

import Rename.Monad
import Rename.Syntactic
import Rename.Type

import Data.Tritraversable

type instance RenameTo (Expr Name a) = Expr UniqueName a
instance {-# OVERLAPPING #-} Renamable (Expr Name a) where
  rename = renameSyntactic (tritraverse rename)
