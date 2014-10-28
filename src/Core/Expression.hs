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

data Expression' = EInteger Int
                 | EChar Char
                 | EVariable QCoreName
                 | EApplication Expression Expression
                 | ELambda CoreName Expression
                 | ELet DeclarationMap Expression
                 | ECase Expression PatternGroup
                 deriving (Show)

data PatternGroupType = DataPatternGroupType QCoreName
                      | IntPatternGroupType
                      | CharPatternGroupType
                      | NoPatternGroupType
                      deriving (Show)

data PatternGroup = PData (Map QCoreName ([CoreName], Expression)) (Maybe Expression)
                  | PInt (Map Int (Expression)) (Maybe Expression)
                  | PChar (Map Char (Expression)) (Maybe Expression)
                  | PNone Expression
                  deriving (Show)

data Expression = Expression
  { expressionType  :: MonoType CoreName
  , expressionValue :: Expression'
  }
  deriving (Show)

expressionFreeVariables :: Expression -> Set CoreName
expressionFreeVariables = expressionFreeVariables' . expressionValue
  where
    expressionFreeVariables' (EInteger _) = Set.empty
    expressionFreeVariables' (EChar _) = Set.empty
    expressionFreeVariables' (EVariable (QName [] VariableName v)) = Set.singleton v
    expressionFreeVariables' (EApplication f t) = Set.union (expressionFreeVariables f) (expressionFreeVariables t)
    expressionFreeVariables' (ELambda x e) = Set.delete x $ expressionFreeVariables e
    expressionFreeVariables' (ELet ds e) = Set.difference
                                           (Set.union
                                            (Set.unions . fmap (\(_, (_, d)) -> declarationFreeVariables d) $ Map.toList ds)
                                            (expressionFreeVariables e)
                                           )
                                           (Set.fromList $ fmap fst $ Map.toList ds)
    expressionFreeVariables' (ECase e p) = Set.union (expressionFreeVariables e) (patternFreeVariables p)

patternFreeVariables :: PatternGroup -> Set CoreName
patternFreeVariables (PData alts df) = Set.union
                                       (Set.unions $ fmap (\(_, (vs, e)) -> Set.difference (expressionFreeVariables e) (Set.fromList vs)) (Map.toList alts))
                                       (maybe Set.empty expressionFreeVariables df)
patternFreeVariables (PInt alts df)  = Set.union
                                       (Set.unions $ fmap (expressionFreeVariables . snd) (Map.toList alts))
                                       (maybe Set.empty expressionFreeVariables df)
patternFreeVariables (PChar alts df) = Set.union
                                       (Set.unions $ fmap (expressionFreeVariables . snd) (Map.toList alts))
                                       (maybe Set.empty expressionFreeVariables df)

declarationFreeVariables :: Declaration -> Set CoreName
declarationFreeVariables (Declaration e)          = expressionFreeVariables e
declarationFreeVariables (PrimitiveDeclaration _) = Set.empty

data Declaration = Declaration Expression
                 | PrimitiveDeclaration S.PrimitiveDeclaration
                 deriving (Show)

type DeclarationMap = Map CoreName (PolyType CoreName, Declaration)


data DataConstructor          = DataConstructor
                                { dataConstructorName      :: QCoreName
                                , dataConstructorArguments :: [MonoType CoreName]
                                , dataConstructorType      :: PolyType CoreName
                                }
                              deriving (Show)
data DataDeclaration          = DataDeclaration [CoreName] [DataConstructor]
                              | PrimitiveDataDeclaration S.PrimitiveDataDeclaration
                              deriving (Show)
type DataDeclarationMap       = Map QCoreName DataDeclaration
