{-# LANGUAGE ViewPatterns #-}

module Parsing.Util where

import Annotation
import Syntactic

import Parsing.Monad

import Syntax.Expression
import Syntax.Type
import Syntax.Name
import Syntax.Module
import Parsing.Location

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Lens

import Data.List
import Data.Maybe
import Data.Monoid
import Data.Foldable

primitiveModule :: ModuleName
primitiveModule = ModuleName ["Primitive"]

primitiveVar :: String -> Name
primitiveVar = Name NsVar primitiveModule

primitiveCon :: String -> Name
primitiveCon = Name NsCon primitiveModule

primitiveTyCon :: String -> Name
primitiveTyCon = Name NsTyCon primitiveModule

tupleTypeName :: Int -> Name
tupleTypeName n = primitiveTyCon (replicate (n-1) ',')

tupleName :: Int -> Name
tupleName n = primitiveCon (replicate (n-1) ',')

makeAbs :: [Pat Name] -> Expr Name () -> ParseMonad (Expr Name ())
makeAbs pats e = do
  let dups = concat (syntacticFreeVariables <$> pats)
             & sort & group
             & filter ((> 1) . length)
             <&> head
  when (not (null dups)) $
    throwError ParseError
  return $ foldr (\pat e -> eAbs (localVar "_") $ eCase (eVar (localVar "_")) [(pat, e)]) e pats
