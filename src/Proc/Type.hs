module Proc.Type where

import Control.Monad
import Control.Applicative

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Type = TInt32
          | TPtr Type
          | TStruct (Map String Type)