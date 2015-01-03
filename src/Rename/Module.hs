{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections #-}

module Rename.Module where

import Annotation
import Syntactic

import Syntax.Module
import Syntax.Name
import Syntax.Type

import Rename.Monad
import Rename.Expression
import Rename.TypeDeclaration

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
import Control.Arrow ((&&&))

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
  let setModule = uniqueName.nameModule .~ mn
  tdecls' <- rename tdecls
             <&> Map.mapKeys setModule
             <&> fmap (dataDeclarationConstructors.traverse.dataConstructorName %~ setModule)
  decls'  <- rename decls
             <&> Map.mapKeys setModule
  return $ Module mn (error "1") (error "2") decls' tdecls' (error "3") (error "4")

runRenameModule :: Map ModuleName (Module UniqueName) -> Module Name -> Either RenameError (Module UniqueName)
runRenameModule mp md = runRename $ do
  imps' <- forM (md ^. moduleImportList) $ \imp -> do
    imd <- Map.lookup (imp ^. importName) mp
           & maybe (error "unknown module") return
    exp <- moduleExportedNames imd
           & return
           -- & maybe return _ (imp ^. importList)
           <&> fmap (view uniqueName &&& RGlobal . (: []))
           <&> Map.fromList
    return $ Map.union
      (if imp ^. importQualified
       then Map.empty
       else exp
            & Map.mapKeys (nameModule .~ localName)
      )
      exp 
  let vars = foldr (Map.unionWith mappend) Map.empty imps'
  bindVariables vars (renameModule md)
  
