{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections, OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveGeneric #-}
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

import GHC.Generics

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Debug.Trace

--

data ModuleImportSpec n = InpVar n
                        | InpAll n
                        | InpFilter n [n]

data ModuleImport n = ModuleImport
                      { _importQualified :: Bool
                      , _importName      :: ModuleName
                      , _importAlias     :: ModuleName
                      , _importList      :: Maybe [ModuleImportSpec n]
                      , _importHiding    :: [ModuleImportSpec n]
                      }
makeLenses ''ModuleImport

--

data ModuleExportSpec n = ExpVar n
                        | ExpAll n
                        | ExpFilter n [n]
                        | ExpModule ModuleName

data DataConstructor n    = DataConstructor
                            { _dataConstructorName  :: n
                            , _dataConstructorTypes :: [Type n ()]
                            }
                          deriving (Generic)
makeLenses ''DataConstructor

data TypeDeclaration n    = DataDeclaration
                            { _typeDeclarationVariables :: [n]
                            , _dataDeclarationConstructors :: [DataConstructor n]
                            }
                          | TypeDeclaration
                            { _typeDeclarationVariables :: [n]
                            , _typeDeclarationAlias :: Type n ()
                            }
                          | PrimitiveTypeDeclaration
                            { _typeDeclarationVariables :: [n]
                            -- , __primitiveTypeKind :: Kind
                            }
                          deriving (Generic)
type TypeDeclarationMap n = Map n (TypeDeclaration n)

makeLenses ''TypeDeclaration

data ClassDeclaration n    = ClassDeclaration
type ClassDeclarationMap n = Map n (ClassDeclaration n)

data InstanceDeclaration n = InstanceDeclaration

data Module n = Module
                { _moduleName                 :: ModuleName
                , _moduleExport               :: Maybe [ModuleExportSpec n]
                , _moduleImportList           :: [ModuleImport n]
                , _moduleDeclarations         :: DeclarationMap n (Expr n ())
                , _moduleTypeDeclarations     :: TypeDeclarationMap n
                , _moduleClassDeclarations    :: ClassDeclarationMap n
                , _moduleInstanceDeclarations :: [InstanceDeclaration n]
                }
makeLenses ''Module

moduleExportedNames :: Show n => Module n -> [n]
moduleExportedNames md = case md ^. moduleExport of
  _ -> concat
             [ md ^. moduleDeclarations
               & Map.keys
             , md ^. moduleTypeDeclarations
               & Map.toList
               <&> (\case
                       (n, TypeDeclaration _ _)      ->
                         [n]
                       (n, DataDeclaration _ cons) ->
                         [n] ++ fmap (view dataConstructorName) cons
                       (n, PrimitiveTypeDeclaration _) ->
                         [n]
                   )
               & concat
             ]
  _ -> error "ICE : module export lists unimplemented"
