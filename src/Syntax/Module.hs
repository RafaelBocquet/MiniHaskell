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

import Text.PrettyPrint.HughesPJ hiding ((<>), empty)
import Text.PrettyPrint.HughesPJClass hiding ((<>), empty)


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
data TypeDeclaration n    = DataDeclaration [n] [DataConstructor n]
                          | TypeDeclaration [n] (Type n ())
type TypeDeclarationMap n = Map n (TypeDeclaration n)

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

instance (Pretty n, Show n) => Pretty (Module n) where
  pPrint (Module mn exps inps decls tdecls cdecls idecls) =
    vcat
    [ text "module" <+> pPrint mn <+> text "where"
    , nest 1 $
      vcat (Map.toList decls
            <&> \(n, e) -> pPrint n <+> text "=" <+> pPrint e
           )
    ]
