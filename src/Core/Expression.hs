module Core.Expression where

import Control.Monad
import Control.Applicative

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Syntax.Type
import Syntax.Name

import qualified Syntax.Expression as S

data Expression' = EInteger Integer
                 | EChar Char
                 | EBool Bool
                 | EVariable (QName CoreName)
                 | EApplication Expression Expression
                 | ELambda CoreName Expression
                 | ETuple [Expression]
                 | EIf Expression Expression Expression
                 | ELet DeclarationMap Expression
                 | EListCase Expression Expression CoreName CoreName Expression
                deriving (Show)
data Expression = Expression
  { expressionType  :: MonoType CoreName
  , expressionValue :: Expression'
  }
  deriving (Show)

data Declaration = Declaration Expression
                 | PrimitiveDeclaration S.PrimitiveDeclaration
                 deriving (Show)

type DeclarationMap = Map CoreName (PolyType CoreName, Declaration)
