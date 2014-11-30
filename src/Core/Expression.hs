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

data AExpression' t = EInteger Int
                    | EChar Char
                    | EVariable QCoreName
                    | EApplication (AExpression t) (AExpression t)
                    | ELambda CoreName (AExpression t)
                    | ELet (ABindingMap t) (AExpression t)
                    | ECase (AExpression t) (APatternGroup t)
                    deriving (Show)
type TypeClassExpression' = AExpression' (MonoType CoreName, [(QCoreName, MonoType CoreName)])
type Expression' = AExpression' (MonoType CoreName)

data PatternGroupType = DataPatternGroupType QCoreName
                      | IntPatternGroupType
                      | CharPatternGroupType
                      | NoPatternGroupType
                      deriving (Show)

data APatternGroup t = PData (Map QCoreName ([CoreName], AExpression t)) (Maybe (AExpression t))
                     | PInt (Map Int (AExpression t)) (Maybe (AExpression t))
                     | PChar (Map Char (AExpression t)) (Maybe (AExpression t))
                     deriving (Show)
type TypeClassPatternGroup = APatternGroup (MonoType CoreName, [(QCoreName, MonoType CoreName)])
type PatternGroup = APatternGroup (MonoType CoreName)

data AExpression t = Expression
  { expressionType  :: t
  , expressionValue :: AExpression' t
  }
  deriving (Show)

type TypeClassExpression = AExpression (MonoType CoreName, [(QCoreName, MonoType CoreName)])
type Expression          = AExpression (MonoType CoreName)

typeclassExpressionType :: TypeClassExpression -> MonoType CoreName
typeclassExpressionType = fst . expressionType

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
                                            (Set.unions . fmap (\(_, (_, d)) -> expressionFreeVariables d) $ Map.toList ds)
                                            (expressionFreeVariables e)
                                           )
                                           (Set.fromList $ fmap fst $ Map.toList ds)
    expressionFreeVariables' (ECase e p) = Set.union (expressionFreeVariables e) (patternFreeVariables p)

makeAbstraction :: [(CoreName, MonoType CoreName)] -> Expression -> Expression
makeAbstraction []             e = e
makeAbstraction ((x, ty) : xs) e =
  let e' = makeAbstraction xs e in
  Expression (makeTypeApplication TyArrow [ty, expressionType e']) (ELambda x e')

makeApplication :: Expression -> [Expression] -> Expression
makeApplication f []       = f
makeApplication f (x : xs) =
  let (TyApplication (TyApplication TyArrow _) b) = expressionType f in
  makeApplication (Expression b (EApplication f x)) xs

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

type ABindingMap t = Map CoreName (PolyType CoreName, AExpression t)
type TypeClassBindingMap = ABindingMap (MonoType CoreName, [(QCoreName, MonoType CoreName)])
type BindingMap = ABindingMap (MonoType CoreName)

data ADeclaration t = Declaration (AExpression t)
                    | PrimitiveDeclaration S.PrimitiveDeclaration
                    deriving (Show)
type TypeClassDeclaration = ADeclaration (MonoType CoreName, [(QCoreName, MonoType CoreName)])
type Declaration = ADeclaration (MonoType CoreName)

type ADeclarationMap t = Map CoreName (PolyType CoreName, ADeclaration t)
type TypeClassDeclarationMap = ADeclarationMap (MonoType CoreName, [(QCoreName, MonoType CoreName)])
type DeclarationMap = ADeclarationMap (MonoType CoreName)


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
