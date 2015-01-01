{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Parsing.Monad where

import Parsing.Location

import Syntax.Name

import Control.Applicative
import Control.Monad
import Control.Lens

import Data.Maybe
import Data.Monoid

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Except
import Control.Monad.State

import Text.PrettyPrint.HughesPJ hiding ((<>), empty)
import Text.PrettyPrint.HughesPJClass hiding ((<>), empty)

data ParseError = ParseError
                deriving (Show)

instance Pretty ParseError where
  pPrint _ = text "parse error"

newtype ParseMonad a = ParseMonad { unParseMonad :: ExceptT ParseError (State Int) a }
                     deriving (MonadError ParseError, MonadState Int, Monad, Functor, Applicative)

runParseMonad :: ParseMonad a -> Either ParseError a
runParseMonad = flip evalState 0 . runExceptT . unParseMonad

freshLocal :: Namespace -> ParseMonad Name
freshLocal ns = do
  i <- id <<%= (+ 1)
  return $ GName ns localName i

freshVar :: ParseMonad Name
freshVar = freshLocal NsVar
