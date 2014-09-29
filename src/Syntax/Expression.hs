module Syntax.Expression where

import Syntax.Location
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

type Binding    = Locate (NameId, [NameId], Expression)
type BindingMap = Map NameId Expression