{-# LANGUAGE TypeFamilies #-}

module Rename.Type where

import Syntax.Type
import Syntax.Name

import Rename.Monad

type instance RenameTo (Type Name a) = Type UniqueName a
