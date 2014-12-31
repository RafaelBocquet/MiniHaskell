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

import Text.PrettyPrint.HughesPJ hiding ((<>), empty)
import Text.PrettyPrint.HughesPJClass hiding ((<>), empty)

-- module names

newtype ModuleName = ModuleName { unModuleName :: [String] }
                   deriving (Ord, Eq, Monoid)

instance Show ModuleName where
  show (ModuleName ns) = foldr1 (\a b -> a ++ '.' : b) ns

instance Pretty ModuleName where
  pPrint mn = text (show mn)

localName :: ModuleName
localName = ModuleName []

-- names

data Namespace = NsVar
               | NsCon
               | NsType
               | NsTyCon
               | NsTyCls
               deriving (Eq, Ord)

data Name = Name
            { _namespace  :: Namespace
            , _nameModule :: ModuleName
            , _name       :: String
            }
--            | GName 
            deriving (Eq, Ord)
makeLenses ''Name

unnamedType :: Name
unnamedType = Name NsType localName ""

instance Pretty Name where
  pPrint (Name _ md mn) = pPrint md <> char '.' <> text mn

-- unique names

data UniqueName = UniqueName
                  { _uniqueId   :: Int
                  , _uniqueName :: Name
                  }
makeLenses ''UniqueName

instance Eq UniqueName where
  UniqueName a _ == UniqueName b _ = a == b
  UniqueName a _ /= UniqueName b _ = a /= b
instance Ord UniqueName where
  UniqueName a _ <  UniqueName b _ = a <  b
  UniqueName a _ <= UniqueName b _ = a <= b
  UniqueName a _ >  UniqueName b _ = a >  b
  UniqueName a _ >= UniqueName b _ = a >= b
instance Pretty UniqueName where
  pPrint (UniqueName id n) = pPrint n
