{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections #-}

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

freeVariablesAnn :: (Syntactic e, Functor (e n), Foldable (e n), Ord n, Monad m) => InductiveAnn (e n) a (Set n) m
freeVariablesAnn = simpleInductiveAnn $ \(Ann _ e) -> do
  return
    $ Set.union
    (syntacticVariables e)
    (foldMap
     (\(bound, Ann (_, subs) _) -> subs `Set.difference` bound)
     (snd (syntacticBinders e))
    )

-- syntacticRename :: (Ord n, Syntactic e, Bifunctor e, Bitraversable e, MonadFresh m) => (n -> RenameMonad n n' n') -> Ann (e n) a -> RenameMonad n n' (Ann (e n') a)
-- syntacticRename fresh (Ann a e) = do
--   let (vs, e') = syntacticBinders e
--   bs <- fmap Map.fromList
--         $ forM (Set.toList vs) $ \v -> fresh v
--                                        & fmap (v,)
--   e'' <- e'
--          & bitraverse
--         (\n -> fromJust . Map.lookup n <$> ask)
--         (\(ws, u) -> do
--             let bs' = Map.filterWithKey (\k _ -> k `Set.member` ws) bs
--             local (Map.union bs') $ syntacticRename fresh u
--         )
--   return $ Ann a e''

