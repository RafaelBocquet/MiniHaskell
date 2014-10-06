module Syntax.Expression where

import Syntax.Location
import Syntax.Name
import Syntax.Type

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
                   | ELet (DeclarationMap n) (Expression n)
                   | EListCase (Expression n) (Expression n) n n (Expression n)
type Expression n = Locate (Expression' n)

expressionFreeVariables :: Ord n => Expression n -> Set n
expressionFreeVariables = expressionFreeVariables' . delocate
  where
    expressionFreeVariables' (EInteger _)                                 =
      Set.empty
    expressionFreeVariables' (EChar _)                                    =
      Set.empty
    expressionFreeVariables' (EVariable (QName [] (Name VariableName v))) =
      Set.singleton v
    expressionFreeVariables' (EVariable _)                                =
      Set.empty
    expressionFreeVariables' (EApplication f t)                           =
      Set.union (expressionFreeVariables f) (expressionFreeVariables t)
    expressionFreeVariables' (ELambda x e)                                =
      Set.delete x (expressionFreeVariables e)
    expressionFreeVariables' (ETuple es)                                  =
      Set.unions $ expressionFreeVariables <$> es
    expressionFreeVariables' (EIf c a b)                                  =
      Set.unions [expressionFreeVariables c, expressionFreeVariables a, expressionFreeVariables b]
    expressionFreeVariables' (ELet bs e)                                  =
      Set.unions (expressionFreeVariables e : (declarationFreeVariables <$> Map.elems bs)) `Set.difference` Set.fromList (Map.keys bs)
    expressionFreeVariables' (EListCase e nil x xs r)                     =
      Set.unions [expressionFreeVariables e, expressionFreeVariables nil, Set.delete x . Set.delete xs $ expressionFreeVariables r]

declarationFreeVariables :: Ord n => Declaration n -> Set n
declarationFreeVariables (Declaration e)          = expressionFreeVariables e
declarationFreeVariables (PrimitiveDeclaration _) = Set.empty

type Binding n    = Locate (n, [n], Expression n)

data PrimitiveDeclaration = FromIntegerDeclaration
                          | NegateDeclaration
                          | AddDeclaration
                          | SubDeclaration
                          | MulDeclaration
                          | DivDeclaration
                          | RemDeclaration
                          | LTDeclaration
                          | LEDeclaration
                          | GTDeclaration
                          | GEDeclaration
                          | EQDeclaration
                          | NEDeclaration
                          | AndDeclaration
                          | OrDeclaration
                          | BindDeclaration
                          | ReturnDeclaration
                          deriving (Show, Eq, Ord)
data Declaration n        = Declaration (Expression n)
                          | PrimitiveDeclaration PrimitiveDeclaration
type DeclarationMap n     = Map n (Declaration n)

data DataConstructor n        = DataConstructor n [MonoType n]
data PrimitiveDataDeclaration = ArrowDataDeclaration
                              | IntDataDeclaration
                              | CharDataDeclaration
                              | BoolDataDeclaration
                              | ListDataDeclaration
                              | UnitDataDeclaration
                              | TupleDataDeclaration Int
                              | IODataDeclaration
                              deriving (Show, Eq, Ord)
data DataDeclaration n        = DataDeclaration [DataConstructor n]
                              | PrimitiveDataDeclaration PrimitiveDataDeclaration
type DataDeclarationMap n     = Map n (DataDeclaration n)