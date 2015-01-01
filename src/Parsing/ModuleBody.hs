{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module Parsing.ModuleBody where

import Parsing.Bindings
import Parsing.Monad

import Syntax.Name
import Syntax.Expression
import Syntax.Type
import Syntax.Module

import Data.List
import Data.Maybe

import Data.Foldable
import Data.Traversable
import Data.Bifunctor

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Lens

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

data ModuleBody = ModuleBody 
                    { _moduleBodyImportList           :: [ModuleImport Name]
                    , _moduleBodyDeclarations         :: Bindings -> ParseMonad Bindings
                    , _moduleBodyTypeDeclarations     :: TypeDeclarationMap Name
                    , _moduleBodyClassDeclarations    :: ClassDeclarationMap Name
                    , _moduleBodyInstanceDeclarations :: [InstanceDeclaration Name]
                    }
makeLenses ''ModuleBody

emptyBody :: ModuleBody
emptyBody = ModuleBody [] return Map.empty Map.empty []

appendBody :: ModuleBody -> ModuleBody -> ParseMonad ModuleBody
ModuleBody imps decls tdecls cdecls idecls `appendBody` ModuleBody imps' decls' tdecls' cdecls' idecls'
  | not (Map.null (Map.intersection tdecls tdecls')) = throwError ParseError
  | not (Map.null (Map.intersection cdecls cdecls')) = throwError ParseError
  | otherwise = return $ ModuleBody (imps ++ imps') (decls >=> decls') (Map.union tdecls tdecls') (Map.union cdecls cdecls') (idecls ++ idecls')

concatBody :: [ModuleBody] -> ParseMonad ModuleBody
concatBody = foldr ((=<<) . appendBody) (return emptyBody)

moduleBodyImport :: [ModuleImport Name] -> ModuleBody
moduleBodyImport imps = emptyBody
                        & moduleBodyImportList .~ imps

moduleBodyTypeDeclaration :: Name -> TypeDeclaration Name -> ModuleBody
moduleBodyTypeDeclaration a b = emptyBody
                                & moduleBodyTypeDeclarations .~ Map.singleton a b

moduleBodyBindingDeclaration :: (Bindings -> ParseMonad Bindings) -> ModuleBody
moduleBodyBindingDeclaration bs = emptyBody
                                & moduleBodyDeclarations .~ bs

