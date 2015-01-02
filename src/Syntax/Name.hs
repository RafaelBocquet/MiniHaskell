{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Syntax.Name where

import Annotation

import Control.Lens
import Control.Arrow

import Data.List
import Data.Maybe
import Data.Monoid

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

-- module names

newtype ModuleName = ModuleName { unModuleName :: [String] }
                   deriving (Ord, Eq, Monoid)

instance Show ModuleName where
  show (ModuleName []) = ""
  show (ModuleName ns) = foldr1 (\a b -> a ++ '.' : b) ns

localName :: ModuleName
localName = ModuleName []

-- names

data Namespace = NsVar
               | NsCon
               | NsTyVar
               | NsTyCon
               deriving (Eq, Ord, Show)

data Name = Name
            { _namespace  :: Namespace
            , _nameModule :: ModuleName
            , _name       :: String
            }
          | GName
            { _namespace :: Namespace
            , _nameModule :: ModuleName
            , _gname :: Int
            }
            deriving (Eq, Ord, Show)
makeLenses ''Name

moduleVar, moduleCon, moduleTyVar, moduleTyCon :: ModuleName -> String -> Name
moduleVar = Name NsVar
moduleCon = Name NsCon
moduleTyVar = Name NsVar
moduleTyCon = Name NsCon

localVar, localCon, localTyVar, localTyCon :: String -> Name
localVar = Name NsVar localName
localCon = Name NsCon localName
localTyVar = Name NsVar localName
localTyCon = Name NsCon localName

-- unnamedType :: Name
-- unnamedType = Name NsTyVar localName ""


-- unique names

data UniqueName = UniqueName
                  { _uniqueId   :: Int
                  , _uniqueName :: Name
                  }
                  deriving (Show)
makeLenses ''UniqueName

instance Eq UniqueName where
  UniqueName a _ == UniqueName b _ = a == b
  UniqueName a _ /= UniqueName b _ = a /= b
instance Ord UniqueName where
  UniqueName a _ <  UniqueName b _ = a <  b
  UniqueName a _ <= UniqueName b _ = a <= b
  UniqueName a _ >  UniqueName b _ = a >  b
  UniqueName a _ >= UniqueName b _ = a >= b
