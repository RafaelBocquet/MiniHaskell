{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Rename.Monad where

import Control.Lens

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Syntax.Name

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

data RenameError = RenameError

newtype Rename a = Rename { unRename :: StateT Int (Reader (Map Name UniqueName)) a }
                 deriving (MonadState Int, MonadReader (Map Name UniqueName), Monad, Applicative, Functor)

runRename :: Rename a -> a
runRename = (runReader ?? Map.empty) . (evalStateT ?? 0) . unRename

fresh :: Name -> Rename UniqueName
fresh n = do
  i <- id <<%= (+ 1)
  return $ UniqueName i n

freshMany :: [Name] -> Rename (Map Name UniqueName)
freshMany ns = fmap Map.fromList
               $ forM ns $ \v -> fresh v
                                 & fmap (v,)

renameLookup :: Name -> Rename UniqueName
renameLookup n = do
  n' <- Map.lookup n <$> view id
  case n' of
   Just n' -> return n'
   Nothing -> error "pouet"

localBind :: Map Name UniqueName -> Rename a -> Rename a
localBind bs = local (Map.union bs)
