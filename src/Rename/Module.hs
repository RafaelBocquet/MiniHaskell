{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections #-}

module Rename.Module where

import Annotation
import Syntactic

import Syntax.Module
import Syntax.Name
import Syntax.Type

import Rename.Monad
import Rename.Declaration

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

  -- _moduleName                 :: ModuleName
  -- _moduleImportList           :: [ModuleImport n]
  -- _moduleDeclarations         :: DeclarationMap n
  -- _moduleTypeDeclarations     :: TypeDeclarationMap n
  -- _moduleClassDeclarations    :: ClassDeclarationMap n
  -- _moduleInstanceDeclarations :: [InstanceDeclaration n]


renameModule :: Module Name -> Rename (Module UniqueName)
renameModule (Module mn exps imps decls tdecls cdecls idecls) = do
  decls' <- renameDeclarationMap decls
  return $ Module mn undefined undefined decls' undefined undefined undefined

