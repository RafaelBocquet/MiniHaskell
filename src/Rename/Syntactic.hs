{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections #-}
{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, TypeFamilies #-}

module Rename.Syntactic where

import Annotation
import Syntactic

import Syntax.Expression
import Syntax.Name

import Rename.Monad

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

type RenameBitraversal e f a = (Name -> Rename UniqueName) ->
                               (([Name], Ann (e Name) a) -> Rename (Ann (f UniqueName) a)) ->
                               (e Name ([Name], Ann (e Name) a)) ->
                               Rename (f UniqueName (Ann (f UniqueName) a))

renameSyntactic :: (Syntactic e) =>
                   RenameBitraversal e f a ->
                   Ann (e Name) a -> Rename (Ann (f UniqueName) a)
renameSyntactic bt (Ann a e) = do
  let (vs, e') = syntacticBinders e
  bs <- fmap Map.fromList
        $ forM vs $ \v -> fresh v
                          & fmap (v,)
  e'' <- e'
         & bt
         (localBind bs . rename)
         (\(Set.fromList -> ws, u) -> do
             let bs' = Map.filterWithKey (\k _ -> k `Set.member` ws) bs
             localBind bs' $ renameSyntactic bt u
         )
  return $ Ann a e''

instance (Syntactic e, Bitraversable e, RenameTo (Ann (e Name) a) ~ Ann (e UniqueName) a) => Renamable (Ann (e Name) a) where
  rename = renameSyntactic bitraverse

