module Syntax.Expression where

import Syntax.Location
import Syntax.Name

import Control.Monad
import Control.Applicative

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Expression' n = EInteger Integer
                   | EChar Char
                   | EVariable (QName n)
                   | EApplication (Expression n) (Expression n)
                   | ELambda n (Expression n)
                   | ETuple [Expression n]
                   | EIf (Expression n) (Expression n) (Expression n)
                   | ELet (BindingMap n) (Expression n)
                   | EListCase (Expression n) (Expression n) n n (Expression n)
type Expression n = Locate (Expression' n)

expressionFreeVariables :: Ord n => Expression n -> Set n
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

type Binding n    = Locate (n, [n], Expression n)
type BindingMap n = Map n (Expression n)