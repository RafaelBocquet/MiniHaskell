{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Rename.Monad where

import Control.Lens

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Syntactic

import Syntax.Name

import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import GHC.Generics

data RenameError = RenameError

newtype Rename a = Rename { unRename :: StateT Int (Reader (Map Name UniqueName)) a }
                 deriving (MonadState Int, MonadReader (Map Name UniqueName), Monad, Applicative, Functor)

runRename :: Rename a -> a
runRename = (runReader ?? Map.empty) . (evalStateT ?? 0) . unRename

-- Renamable class

type family RenameTo a

class Renamable a where
  rename :: a -> Rename (RenameTo a)

-- generic instances

class GRenamable a where
  type GRenameTo a :: * -> *
  grename :: a x -> Rename (GRenameTo a x)

instance GRenamable U1 where
  type GRenameTo U1 = U1
  grename U1 = return U1

instance (GRenamable a, GRenamable b) => GRenamable (a :*: b) where
  type GRenameTo (a :*: b) = (GRenameTo a :*: GRenameTo b)
  grename (a :*: b) = liftM2 (:*:) (grename a) (grename b) 

instance (GRenamable a, GRenamable b) => GRenamable (a :+: b) where
  type GRenameTo (a :+: b) = (GRenameTo a :+: GRenameTo b)
  grename (L1 a) = L1 <$> grename a
  grename (R1 a) = R1 <$> grename a

instance Renamable a => GRenamable (K1 i a) where
  type GRenameTo (K1 i a) = K1 i (RenameTo a)
  grename (K1 a) = K1 <$> rename a

instance GRenamable a => GRenamable (M1 i c a) where
  type GRenameTo (M1 i c a) = M1 i c (GRenameTo a)
  grename (M1 a) = M1 <$> grename a

instance {-# OVERLAPPABLE #-} (Generic a, GRenamable (Rep a), Generic (RenameTo a), Rep (RenameTo a) ~ GRenameTo (Rep a), SimpleSyntactic a Name) => Renamable a where
  rename x = do
    bs <- freshMany (syntacticBound x)
    localBind (Map.fromList bs) $ GHC.Generics.to <$> grename (GHC.Generics.from x)

type instance RenameTo [a]  = [RenameTo a]
instance Renamable a => Renamable [a] where
  rename = mapM rename 

type instance RenameTo Name = UniqueName
instance Renamable Name where
  rename = renameLookup

--

fresh :: Name -> Rename UniqueName
fresh n = do
  i <- id <<%= (+ 1)
  return $ UniqueName i n

freshMany :: [Name] -> Rename [(Name, UniqueName)]
freshMany ns = forM ns $ \v -> fresh v
                                 & fmap (v,)

renameMap :: (a -> Rename b) -> Map Name a -> Rename (Map UniqueName b)
renameMap f mp = do 
  bs <- Map.fromList <$> freshMany (Map.keys mp)
  localBind bs
    $ (Map.mapKeys (fromJust . flip Map.lookup bs) mp)
    & traverse f

renameLookup :: Name -> Rename UniqueName
renameLookup n = do
  env <- view id
  n' <- Map.lookup n <$> view id
  case n' of
   Just n' -> return n'
   Nothing -> error $ "unknown name " ++ show n ++ " in env \n" ++ show env

localBind :: Map Name UniqueName -> Rename a -> Rename a
localBind bs = local (Map.union bs)
