{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections #-}
{-# LANGUAGE TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}

module Rename.Type where

import Syntactic

import Syntax.Type
import Syntax.Name

import Rename.Syntactic
import Rename.Monad

import Control.Lens

import Data.Bitraversable

type instance RenameTo (Type Name a) = Type UniqueName a
instance {-# OVERLAPPING #-} Renamable (Type Name a) where
  rename = renameSyntactic . flip
           $ \f -> bitraverse
                   ?? (\case
                          -- TODO : handle type aliases here
                          x -> f x
                      )

-- TODO : Open -> ???

renameOpenType :: Type Name () -> Rename (Type UniqueName ())
renameOpenType ty = let vs = syntacticFreeVariables ty
                    in rename (makeTypeForall vs ty)
