{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections #-}

module Rename.Expression where

import Annotation
import Syntactic

import Syntax.Expression
import Syntax.Name
import Syntax.Type

import Rename.Monad
import Rename.Syntactic
import Rename.Type

import Data.Maybe

import Data.Functor
import Data.Foldable
import Data.Traversable
import Data.Trifunctor
import Data.Trifoldable
import Data.Tritraversable

import Control.Applicative
import Control.Monad hiding (forM)
import Control.Monad.State hiding (forM)
import Control.Monad.Reader hiding (forM)

import Control.Lens

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

renameExpression :: Expr Name a -> Rename (Expr UniqueName a)
renameExpression = renameSyntactic (tritraverse renameType)
