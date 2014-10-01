module Syntax.Expression where

import Syntax.Location
import Syntax.Name

import Control.Monad
import Control.Applicative

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Expression' = EInteger Integer
                 | EChar Char
                 | EVariable QName
                 | EApplication Expression Expression
                 | ELambda NameId Expression
                 | ETuple [Expression]
                 | EIf Expression Expression Expression
                 | ELet BindingMap Expression
                 | EListCase Expression Expression NameId NameId Expression
type Expression = Locate Expression'

expressionFreeVariables :: Expression -> Set NameId
expressionFreeVariables = expressionFreeVariables' . delocate
  where
    expressionFreeVariables' (EInteger _)                                 = Set.empty
    expressionFreeVariables' (EChar _)                                    = Set.empty
    expressionFreeVariables' (EVariable (QName [] (Name VariableName v))) = Set.singleton v
    expressionFreeVariables' (EVariable _)                                = Set.empty
    expressionFreeVariables' (EApplication f t)                           = Set.union (expressionFreeVariables f) (expressionFreeVariables t)
    expressionFreeVariables' (ELambda x e)                                = Set.delete x (expressionFreeVariables e)
    expressionFreeVariables' (ETuple es)                                  = Set.unions $ expressionFreeVariables <$> es
    expressionFreeVariables' (EIf c a b)                                  = Set.unions [expressionFreeVariables c, expressionFreeVariables a, expressionFreeVariables b]
    expressionFreeVariables' (ELet bs e)                                  = Set.unions (expressionFreeVariables <$> e : Map.elems bs) `Set.difference` Set.fromList (Map.keys bs)
    expressionFreeVariables' (EListCase e nil x xs r)                     = Set.unions [expressionFreeVariables e, expressionFreeVariables nil, Set.delete x . Set.delete xs $ expressionFreeVariables r]

type Binding    = Locate (NameId, [NameId], Expression)
type BindingMap = Map NameId Expression