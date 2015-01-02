{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleInstances #-}

module Rename.TypeDeclaration where

import Syntactic

import Control.Lens

import Syntax.Name
import Syntax.Module

import Rename.Monad

type instance RenameTo (DataConstructor Name) = DataConstructor UniqueName
instance SimpleSyntactic (DataConstructor n) n where
  syntacticBound x = [x ^. dataConstructorName]

type instance RenameTo (TypeDeclaration Name) = TypeDeclaration UniqueName
instance SimpleSyntactic (TypeDeclaration n) n where
  syntacticBound x = x ^. typeDeclarationVariables

-- renameDataConstructor :: 

-- renameTypeDeclaration :: TypeDeclaration Name -> Rename (TypeDeclaration UniqueName)
-- renameTypeDeclaration (TypeDeclaration vs ty) = do
--   bs <- freshMany $ vs
--   TypeDeclaration (snd <$> bs)
--     <$> localBind (Map.fromList bs) (rename ty)
-- renameTypeDeclaration (DataDeclaration vs cons) = do
--   bs <- freshMany $ vs
--   fmap DataDeclaration (snd <$> bs)
--     $ localBind (Map.fromList bs)
--     $ cons
--     & traverse renameDataConstructor
