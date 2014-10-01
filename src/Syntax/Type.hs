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

data MonoType = TyApplication NameId [MonoType]
              | TyVariable NameId

instance Show MonoType where
  show (TyApplication (UserName "()") [])     = "()"
  show (TyApplication (UserName "[]") [a])    = "[" ++ show a ++ "]"
  show (TyApplication (UserName "->") [a, b]) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show (TyApplication name [])                = show name
  show (TyApplication name ms)                = "(" ++ show name ++ concat ((' ' :) . show <$> ms) ++ ")"
  show (TyVariable v)                         = show v

tyList :: MonoType -> MonoType
tyList a = TyApplication (UserName "[]") [a]

tyIO :: MonoType -> MonoType
tyIO a = TyApplication (UserName "IO") [a]

tyUnit :: MonoType
tyUnit = TyApplication (UserName "()") []

tyInteger :: MonoType
tyInteger = TyApplication (UserName "Integer") []

tyBool :: MonoType
tyBool = TyApplication (UserName "Bool") []

tyChar :: MonoType
tyChar = TyApplication (UserName "Char") []

tyArrow :: MonoType -> MonoType -> MonoType
tyArrow a b = TyApplication (UserName "->") [a, b]

tyArrowList :: [MonoType] -> MonoType -> MonoType
tyArrowList [] b     = b
tyArrowList (a:as) b = tyArrow a (tyArrowList as b)

data PolyType = PolyType
  { polyTypeVariables :: Set NameId
  , polyTypeType      :: MonoType
  }

instance Show PolyType where
  show (PolyType vs t) | Set.null vs = show t
  show (PolyType vs t)               = "forall" ++ concat ((' ' :) . show <$> Set.toList vs) ++ ". " ++ show t

type Type     = PolyType

freeTypeVariables :: MonoType -> Set NameId
freeTypeVariables (TyVariable n)       = Set.singleton n
freeTypeVariables (TyApplication d ts) = Set.unions $ freeTypeVariables <$> ts

freePolyTypeVariables :: PolyType -> Set NameId
freePolyTypeVariables (PolyType vs t) = Set.difference (freeTypeVariables t) vs