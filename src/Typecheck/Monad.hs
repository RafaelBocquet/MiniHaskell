{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}

module Typecheck.Monad where

import Control.Lens

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Annotation

import Syntax.Name
import Syntax.Type

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

-- data TypecheckError = TypecheckError

-- data TypecheckState = TypecheckState
--                       { _typecheckFresh :: Int
--                       , _typecheckConstraints :: [Constraint UniqueName ()]
--                       }
-- makeLenses ''TypecheckState

-- data TypecheckEnv = TypecheckEnv
--                     { _typecheckVariables :: Map UniqueName (Type UniqueName ())
--                     }
-- makeLenses ''TypecheckEnv

-- -- 

-- newtype Typecheck a = Typecheck { runTypecheck :: StateT TypecheckState (Reader TypecheckEnv) a }
--                     deriving (MonadState TypecheckState, MonadReader TypecheckEnv, Monad, Applicative, Functor)

-- freshTypeVariable :: Typecheck (Type UniqueName ())
-- freshTypeVariable = do
--   i <- typecheckFresh <<%= (+ 1)
--   return $ ann $ TyVar' (UniqueName i unnamedType)

-- bindVariable :: UniqueName -> Type UniqueName () -> Typecheck a -> Typecheck a
-- bindVariable x t = local (over typecheckVariables $ Map.insert x t)

-- bindVariables :: Map UniqueName (Type UniqueName ()) -> Typecheck a -> Typecheck a
-- bindVariables ts = local (over typecheckVariables $ Map.union ts)

-- addConstraint :: Constraint UniqueName () -> Typecheck ()
-- addConstraint c = typecheckConstraints %= (c:)

-- unify :: Type UniqueName () -> Type UniqueName () -> Typecheck ()
-- unify a b = addConstraint (CsUnify a b)

-- instantiate :: Type UniqueName () -> Typecheck (Type UniqueName ())
-- instantiate t = do
--   tau <- freshTypeVariable
--   addConstraint (CsInstance tau t)
--   return tau
