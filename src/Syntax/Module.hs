{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module Syntax.Module where

import Annotation

import Syntax.Name
import Syntax.Expression
import Syntax.Type

import Data.List
import Data.Maybe

import Data.Foldable
import Data.Traversable
import Data.Bifunctor

import Control.Applicative
import Control.Monad

import Control.Lens

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

data ModuleImport n = ModuleImport
                      { _importQualified :: Bool
                      , _importName      :: ModuleName
                      , _importAlias     :: ModuleName
                      , _importList      :: Maybe [n]
                      , _importHiding    :: [n]
                      }
makeLenses ''ModuleImport

data DataConstructor n    = DataConstructor
                            { _dataConstructorName :: n
                            , _dataConstructorTypes :: [Type n ()]
                            }
data TypeDeclaration n    = DataDeclaration [n] [DataConstructor n]
                          | TypeDeclaration [n] (Type n ())
type TypeDeclarationMap n = Map n (TypeDeclaration n)

data ClassDeclaration n    = ClassDeclaration
type ClassDeclarationMap n = Map n (ClassDeclaration n)

data InstanceDeclaration n = InstanceDeclaration

data Module n = Module
                { _moduleName                 :: ModuleName
                , _moduleImportList           :: [ModuleImport n]
                , _moduleDeclarations         :: DeclarationMap n (Expr (Type n ()) n ())
                , _moduleTypeDeclarations     :: TypeDeclarationMap n
                , _moduleClassDeclarations    :: ClassDeclarationMap n
                , _moduleInstanceDeclarations :: [InstanceDeclaration n]
                }
makeLenses ''Module
  
data ModuleBody n = ModuleBody 
                    { _moduleBodyImportList           :: [ModuleImport n]
                    , _moduleBodyDeclarations         :: DeclarationMap n (Expr (Type n ()) n ())
                    , _moduleBodyTypeDeclarations     :: TypeDeclarationMap n
                    , _moduleBodyClassDeclarations    :: ClassDeclarationMap n
                    , _moduleBodyInstanceDeclarations :: [InstanceDeclaration n]
                    }
makeLenses ''ModuleBody

emptyBody :: ModuleBody n
emptyBody = ModuleBody [] Map.empty Map.empty Map.empty []

appendBody :: Ord n => ModuleBody n -> ModuleBody n -> Maybe (ModuleBody n)
ModuleBody imps decls tdecls cdecls idecls `appendBody` ModuleBody imps' decls' tdecls' cdecls' idecls'
  | not (Map.null (Map.intersection decls decls'))   = Nothing
  | not (Map.null (Map.intersection tdecls tdecls')) = Nothing
  | not (Map.null (Map.intersection cdecls cdecls')) = Nothing
  | otherwise = Just $ ModuleBody (imps ++ imps') (Map.union decls decls') (Map.union tdecls tdecls') (Map.union cdecls cdecls') (idecls ++ idecls')

concatBody :: Ord n => [ModuleBody n] -> Maybe (ModuleBody n)
concatBody = foldr ((=<<) . appendBody) (Just emptyBody)

moduleBodyTypeDeclaration :: n -> TypeDeclaration n -> ModuleBody n
moduleBodyTypeDeclaration a b = ModuleBody [] Map.empty (Map.singleton a b) Map.empty []
