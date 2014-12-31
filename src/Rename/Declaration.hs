{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections #-}

module Rename.Declaration where

import Annotation
import Syntactic

import Syntax.Expression
import Syntax.Type
import Syntax.Name

import Rename.Monad
import Rename.Syntactic
import Rename.Expression
import Rename.Type

import Data.Maybe

import Data.Functor
import Data.Foldable
import Data.Traversable
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable

import Control.Applicative
import Control.Monad hiding (forM)
import Control.Monad.State hiding (forM)
import Control.Monad.Reader hiding (forM)

import Control.Lens

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

renameDeclarationMap :: DeclarationMap Name (Expr (Type Name ()) Name ()) -> Rename (DeclarationMap UniqueName (Expr (Type UniqueName ()) UniqueName ()))
renameDeclarationMap ds = do
  bs <- freshMany $ Map.keys ds
  localBind bs
    $ (Map.mapKeys (fromJust . flip Map.lookup bs) ds)
    & traverse renameExpression
