{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
  
module Rename.Monad where

import Control.Lens

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Syntactic

import Syntax.Name
import Syntax.Type

import Data.Maybe

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import GHC.Generics

import Text.PrettyPrint.HughesPJ hiding ((<>), empty)
import Text.PrettyPrint.HughesPJClass hiding ((<>), empty)

data RenameError = RenameError
                 deriving (Show)

instance Pretty RenameError where
  pPrint = text . show

data RenameVariable = RNone
                    | RGlobal [UniqueName]
                    | RLocal UniqueName
                    deriving (Show)

instance Monoid RenameVariable where
  mempty = RNone
  RNone `mappend` a               = a
  a `mappend` RNone               = a
  RGlobal xs `mappend` RGlobal ys = RGlobal (xs ++ ys)
  RLocal a `mappend` _            = RLocal a
  _ `mappend` RLocal a            = error "ICE"
  
data RenameEnv = RenameEnv
                 { _renameVariables :: Map Name RenameVariable
                 , _renameTypeAlias :: Map UniqueName (Type UniqueName ())
                 }
                 deriving (Show)

makeLenses ''RenameEnv

newtype Rename a = Rename { unRename :: StateT Int (ReaderT RenameEnv (Except RenameError)) a }
                 deriving (MonadState Int, MonadReader RenameEnv, MonadError RenameError, Monad, Applicative, Functor)

runRename :: Rename a -> Either RenameError a
runRename = runExcept . (runReaderT ?? RenameEnv Map.empty Map.empty) . (evalStateT ?? 0) . unRename

-- Renamable class

type family RenameTo a

class Renamable a where
  rename :: a -> Rename (RenameTo a)

-- Generic instances

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

-- This case is overlappable so as not to be a possible match in all cases

instance {-# OVERLAPPABLE #-} (Generic a, GRenamable (Rep a), Generic (RenameTo a), Rep (RenameTo a) ~ GRenameTo (Rep a), SimpleSyntactic a Name) => Renamable a where
  rename x = do
    bs <- freshMany (syntacticBound x)
    localBind (Map.fromList bs) $ GHC.Generics.to <$> grename (GHC.Generics.from x)

-- Common instances

type instance RenameTo [a]  = [RenameTo a]
instance Renamable a => Renamable [a] where
  rename = mapM rename 

type instance RenameTo Name = UniqueName
instance Renamable Name where
  rename n = do
    n' <- Map.lookup n <$> view renameVariables
    case n' of
     Just (RGlobal [n'])                -> return n'
     Just (RLocal n')                   -> return n'
     Just (RGlobal ns) | length ns >= 2 -> error "e2"
     _                                  -> error $ "unknown name " ++ show n

-- Consider all the keys as fresh, which are then bound when renaming the elements
type instance RenameTo (Map Name a) = Map UniqueName (RenameTo a)
instance Renamable a => Renamable (Map Name a) where
  rename mp = do 
    bs <- Map.fromList <$> freshMany (Map.keys mp)
    localBind bs
      $ (Map.mapKeys (fromJust . flip Map.lookup bs) mp)
      & traverse rename

--

fresh :: Name -> Rename UniqueName
fresh n = do
  i <- id <<%= (+ 1)
  return $ UniqueName i n

freshMany :: [Name] -> Rename [(Name, UniqueName)]
freshMany ns = forM ns $ \v -> fresh v
                                 & fmap (v,)

bindVariables :: Map Name RenameVariable -> Rename a -> Rename a
bindVariables bs = local (renameVariables %~ Map.unionWith mappend bs)

localBind :: Map Name UniqueName -> Rename a -> Rename a
localBind = bindVariables . fmap RLocal
