{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections #-}

module Rename.Expression where

import Annotation
import Syntactic

import Syntax.Expression
import Syntax.Name
import Syntax.Type

import Rename.Monad
import Rename.Syntactic

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

renameExpression :: Ann (Expr' (Type Name ()) Name) a -> Rename (Ann (Expr' (Type UniqueName ()) UniqueName) a)
renameExpression (Ann a e) = do
  let (vs, e') = syntacticBinders e
  bs <- fmap Map.fromList
        $ forM (Set.toList vs) $ \v -> fresh v
                                       & fmap (v,)
  e'' <- e'
         & tritraverse
         renameSyntactic
         renameLookup
         (\(ws, u) -> do
             let bs' = Map.filterWithKey (\k _ -> k `Set.member` ws) bs
             localBind bs $ renameExpression u
         )
  return $ Ann a e''
