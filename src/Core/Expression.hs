module Core.Expression where

import Control.Monad
import Control.Applicative

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Syntax.Type

data Expression' = EInteger Integer
                 | EChar Char
                 | EBool Bool
                 | EVariable Int
                 | EApplication Expression Expression
                 | ELambda Int Expression
                 | ETuple [Expression]
                 | EIf Expression Expression Expression
                 | ELet BindingMap Expression
                 | EListCase Expression Expression Int Int Expression
                deriving (Show)
data Expression = Expression
  { expressionType  :: MonoType
  , expressionValue :: Expression'
  }
  deriving (Show)

type BindingMap = Map Int (PolyType, Expression)