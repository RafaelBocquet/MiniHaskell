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
                   deriving (Show)
type Expression n = Locate (Expression' n)

expressionFreeVariables :: Ord n => Expression n -> Set (QName n)
expressionFreeVariables = expressionFreeVariables' . delocate
  where
    expressionFreeVariables' (EInteger _)                                 =
      Set.empty
    expressionFreeVariables' (EChar _)                                    =
      Set.empty
    expressionFreeVariables' (EVariable v)                                =
      Set.singleton v
    expressionFreeVariables' (EApplication f t)                           =
      Set.union (expressionFreeVariables f) (expressionFreeVariables t)
    expressionFreeVariables' (ELambda x e)                                =
      Set.delete (QName [] VariableName x) (expressionFreeVariables e)
    expressionFreeVariables' (ETuple es)                                  =
      Set.unions $ expressionFreeVariables <$> es
    expressionFreeVariables' (EIf c a b)                                  =
      Set.unions [expressionFreeVariables c, expressionFreeVariables a, expressionFreeVariables b]
    expressionFreeVariables' (ELet bs e)                                  =
      Set.unions (expressionFreeVariables e : (declarationFreeVariables <$> Map.elems bs)) `Set.difference` Set.map (QName [] VariableName) (Map.keysSet bs)
    expressionFreeVariables' (EListCase e nil x xs r)                     =
      Set.unions [ expressionFreeVariables e
                 , expressionFreeVariables nil
                 , Set.delete (QName [] VariableName x) . Set.delete (QName [] VariableName xs) $ expressionFreeVariables r
                 ]

declarationFreeVariables :: Ord n => Declaration n -> Set (QName n)
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
                          | ErrorDeclaration
                          | PutCharDeclaration
                          deriving (Show, Eq, Ord)
data Declaration n        = Declaration (Expression n)
                          | PrimitiveDeclaration PrimitiveDeclaration
                          deriving (Show)
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
data DataDeclaration n        = DataDeclaration [n] [DataConstructor n]
                              | PrimitiveDataDeclaration PrimitiveDataDeclaration
type DataDeclarationMap n     = Map n (DataDeclaration n)