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

data Expression' n = EInteger Int
                   | EChar Char
                   | EVariable (QName n)
                   | EApplication (Expression n) (Expression n)
                   | ELambda n (Expression n)
                   | ETuple [Expression n]
                   | ELet (DeclarationMap n) (Expression n)
                   | ECase (Expression n) [(Pattern n, Expression n)]
                   deriving (Show)
type Expression n = Locate (Expression' n)

makeApplication = foldl (\a b -> Locate noLocation $ EApplication a b)

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
    expressionFreeVariables' (ELet bs e)                                  =
      Set.unions (expressionFreeVariables e : (declarationFreeVariables <$> Map.elems bs)) `Set.difference` Set.map (QName [] VariableName) (Map.keysSet bs)
    expressionFreeVariables' (ECase e pats)                               =
      Set.union
        (expressionFreeVariables e)
        (Set.unions $ fmap (\(pat, pate) -> expressionFreeVariables pate `Set.difference` patternVariables pat) pats)

declarationFreeVariables :: Ord n => Declaration n -> Set (QName n)
declarationFreeVariables (Declaration _ e)        = expressionFreeVariables e
declarationFreeVariables (PrimitiveDeclaration _) = Set.empty

data Pattern n = Pattern (Pattern' n) [n]
               deriving (Show, Eq, Ord)

data Pattern' n = PWildcard
                | PConstructor (QName n) [Pattern n]
                | PLiteralInt Int
                | PLiteralChar Char
                deriving (Show, Eq, Ord)

patternSkeleton :: Pattern n -> Pattern n
patternSkeleton (Pattern p _) = Pattern (patternSkeleton' p) []

patternSkeleton' :: Pattern' n -> Pattern' n
patternSkeleton' PWildcard             = PWildcard
patternSkeleton' (PConstructor n pats) = PConstructor n $ patternSkeleton <$> pats
patternSkeleton' p@(PLiteralInt _)     = p
patternSkeleton' p@(PLiteralChar _)    = p

-- PVariable v ~ PAs v PWildcard

patternVariables :: Ord n => Pattern n -> Set (QName n)
patternVariables (Pattern pat vs)      = Set.union (Set.fromList $ (QName [] VariableName) <$> vs) (patternVariables' pat)

patternVariables'  :: Ord n => Pattern' n -> Set (QName n)
patternVariables' PWildcard             = Set.empty
patternVariables' (PConstructor _ pats) = Set.unions $ patternVariables <$> pats
patternVariables' (PLiteralInt _)       = Set.empty
patternVariables' (PLiteralChar _)      = Set.empty

data Fixity = Infix | Infixl | Infixr

type Binding n    = Locate (n, [n], Expression n)

data PrimitiveDeclaration = PrimitiveIntAdd
                          | PrimitiveIntSub
                          | PrimitiveIntMul
                          | PrimitiveIntDiv
                          | PrimitiveIntRem
                          | PrimitiveIntNegate
                          | PrimitiveIntLT
                          | PrimitiveIntLE
                          | PrimitiveIntGT
                          | PrimitiveIntGE
                          | PrimitiveIntEQ
                          | PrimitiveIntNE
                          | PrimitiveOrd
                          | PrimitiveChr
                          | PrimitiveCharLT
                          | PrimitiveCharLE
                          | PrimitiveCharGT
                          | PrimitiveCharGE
                          | PrimitiveCharEQ
                          | PrimitiveCharNE
                          | PrimitiveBindIO
                          | PrimitiveReturnIO
                          | PrimitivePutChar
                          | PrimitiveError
                          deriving (Show, Eq, Ord)
                          
data Declaration n        = Declaration (Maybe (MonoType n)) (Expression n)
                          | PrimitiveDeclaration PrimitiveDeclaration
                          deriving (Show)
type DeclarationMap n     = Map n (Declaration n)


data DataConstructor n        = DataConstructor n [MonoType n]
                              deriving (Show)
data PrimitiveDataDeclaration = UnboxedIntDataDeclaration
                              | UnboxedCharDataDeclaration
                              | IODataDeclaration
                              deriving (Show, Eq, Ord)
data TypeDeclaration n        = DataDeclaration [n] [DataConstructor n]
                              | PrimitiveDataDeclaration PrimitiveDataDeclaration
                              | TypeDeclaration [n] (MonoType n)
                              deriving (Show)
type TypeDeclarationMap n     = Map n (TypeDeclaration n)

data ClassDeclaration n    = ClassDeclaration n (Map n (MonoType n))
                           deriving (Show)
type ClassDeclarationMap n = Map n (ClassDeclaration n)

data InstanceDeclaration n    = InstanceDeclaration [n] (DeclarationMap n)
                              deriving (Show)
type InstanceDeclarationMap n = Map (QName n, MonoType n) (InstanceDeclaration n)  
