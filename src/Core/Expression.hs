module Core.Expression where

import Control.Monad
import Control.Applicative

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Syntax.Type
import Syntax.Name

data Expression' = EInteger Integer
                 | EChar Char
                 | EBool Bool
                 | EVariable CoreName
                 | EApplication Expression Expression
                 | ELambda CoreName Expression
                 | ETuple [Expression]
                 | EIf Expression Expression Expression
                 | ELet BindingMap Expression
                 | EListCase Expression Expression CoreName CoreName Expression
                deriving (Show)
data Expression = Expression
  { expressionType  :: MonoType CoreName
  , expressionValue :: Expression'
  }
  deriving (Show)

type BindingMap = Map Int (PolyType CoreName, Expression)
