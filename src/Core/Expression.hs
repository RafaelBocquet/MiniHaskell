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

data Expression' = EInteger Integer                                             -- OK
                 | EChar Char                                                   -- OK
                 | EBool Bool                                                   -- OK
                 | EVariable QCoreName                                          -- ~OK
                 | EApplication Expression Expression                           -- ~OK
                 | ELambda CoreName Expression                                  -- ~OK
                 | ETuple [Expression]                                          -- OK
                 | ELet DeclarationMap Expression                               -- ~OK
                 | ECase Expression PatternGroup
                 deriving (Show)

data PatternGroupType = DataPatternGroupType QCoreName
                      | IntPatternGroupType
                      | CharPatternGroupType
                      | NoPatternGroupType
                      deriving (Show)

data PatternGroup = PData (Map CoreName ([CoreName], Expression)) (Maybe Expression)
                  | PInt (Map Int Expression) (Maybe Expression)
                  | PChar (Map Char Expression) (Maybe Expression)
                  | PNone Expression
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

data DataDeclaration    = NONENONENONE
type DataDeclarationMap = Map CoreName DataDeclaration
