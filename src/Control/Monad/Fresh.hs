{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Fresh where

import Control.Lens

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Syntax.Name

data TypecheckError = TypecheckError

data TypecheckState =

newtype FreshT a = FreshT { runState :: StateT Int (Reader (Map Name UniqueName)) a }
                 deriving (MonadState Int, MonadReader (Map Name UniqueName), Monad, Applicative, Functor)
