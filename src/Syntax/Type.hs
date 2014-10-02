module Syntax.Type where

import Syntax.Name

import Control.Monad
import Control.Applicative

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Kind = KStar
          | KArrow Kind Kind

instance Show Kind where
  show (KArrow k1 k2) = "(" ++ show k1 ++ " -> " ++ show k2 ++ ")"
  show KStar          = "*"

data MonoType n = TyApplication (QName n) [MonoType n]
                | TyVariable n

instance Show n => Show (MonoType n) where
  --show (TyApplication (UserName "()") [])     = "()"
  --show (TyApplication (UserName "[]") [a])    = "[" ++ show a ++ "]"
  --show (TyApplication (UserName "->") [a, b]) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show (TyApplication name [])                = show name
  show (TyApplication name ms)                = "(" ++ show name ++ concat ((' ' :) . show <$> ms) ++ ")"
  show (TyVariable v)                         = show v

tyList :: MonoType SyntaxName -> MonoType SyntaxName
tyList a = TyApplication (QName [] $ Name ConstructorName $ UserName "[]") [a]

tyTuple :: [MonoType SyntaxName] -> MonoType SyntaxName
tyTuple xs = TyApplication (QName [] $ Name ConstructorName $ UserName $ replicate (length xs - 1) ',') xs

tyIO :: MonoType SyntaxName -> MonoType SyntaxName
tyIO a = TyApplication (QName [] $ Name ConstructorName $ UserName "IO") [a]

tyUnit :: MonoType SyntaxName
tyUnit = TyApplication (QName [] $ Name ConstructorName $ UserName "()") []

tyInteger :: MonoType SyntaxName
tyInteger = TyApplication (QName [] $ Name ConstructorName $ UserName "Integer") []

tyBool :: MonoType SyntaxName
tyBool = TyApplication (QName [] $ Name ConstructorName $ UserName "Bool") []

tyChar :: MonoType SyntaxName
tyChar = TyApplication (QName [] $ Name ConstructorName $ UserName "Char") []

tyArrow :: MonoType SyntaxName -> MonoType SyntaxName -> MonoType SyntaxName
tyArrow a b = TyApplication (QName [] $ Name ConstructorName $ UserName "->") [a, b]

tyArrowList :: [MonoType SyntaxName] -> MonoType SyntaxName -> MonoType SyntaxName
tyArrowList [] b     = b
tyArrowList (a:as) b = tyArrow a (tyArrowList as b)

data PolyType n = PolyType
  { polyTypeVariables :: Set n
  , polyTypeType      :: MonoType n
  }

instance Show n => Show (PolyType n) where
  show (PolyType vs t) | Set.null vs = show t
  show (PolyType vs t)               = "forall" ++ concat ((' ' :) . show <$> Set.toList vs) ++ ". " ++ show t

freeTypeVariables :: Ord n => MonoType n -> Set n
freeTypeVariables (TyVariable n)       = Set.singleton n
freeTypeVariables (TyApplication d ts) = Set.unions $ freeTypeVariables <$> ts

freePolyTypeVariables :: Ord n => PolyType n -> Set n
freePolyTypeVariables (PolyType vs t) = Set.difference (freeTypeVariables t) vs