module Syntax.Module where

import Syntax.Location
import Syntax.Name
import Syntax.Expression

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Module n = Module [String] (BindingMap n)