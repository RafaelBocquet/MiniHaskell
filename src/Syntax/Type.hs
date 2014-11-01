module Syntax.Type where

import Syntax.Name

import Control.Monad
import Control.Applicative

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Kind n = KStar
            | KVariable n
            | KArrow (Kind n) (Kind n)

instance Show n => Show (Kind n) where
  show (KArrow k1 k2) = "(" ++ show k1 ++ " -> " ++ show k2 ++ ")"
  show KStar          = "*"
  show (KVariable v)  = show v

data MonoType n = TyApplication (MonoType n) (MonoType n)
                | TyVariable n
                | TyConstant (QName n)
                | TyArrow

instance Show n => Show (MonoType n) where
  --show (TyApplication (UserName "()") [])     = "()"
  --show (TyApplication (UserName "[]") [a])    = "[" ++ show a ++ "]"
  --show (TyApplication (UserName "->") [a, b]) = "(" ++ show a ++ " -> " ++ show b ++ ")"
  show (TyApplication a b) = "(" ++ show a ++ " " ++ show b ++ ")"
  show (TyVariable v)      = show v
  show (TyConstant c)      = show c
  show TyArrow             = "(->)"

data PolyType n = PolyType
  { polyTypeVariables   :: Set n
  , polyTypeType        :: MonoType n
  }

instance Show n => Show (PolyType n) where
  show (PolyType vs t) | Set.null vs = show t
  show (PolyType vs t)               = "forall" ++ concat ((' ' :) . show <$> Set.toList vs) ++ ". " ++ show t

makeTypeApplication :: Ord n => MonoType n -> [MonoType n] -> MonoType n
makeTypeApplication = foldl TyApplication

freeKindVariables :: Ord n => Kind n -> Set n
freeKindVariables (KVariable v)                     = Set.singleton v
freeKindVariables KStar                             = Set.empty
freeKindVariables (KArrow a b)                      = Set.union (freeKindVariables a) (freeKindVariables b)


freeTypeVariables :: Ord n => MonoType n -> Set n
freeTypeVariables (TyVariable v)                             = Set.singleton v
freeTypeVariables (TyConstant _)                             = Set.empty
freeTypeVariables TyArrow                                    = Set.empty
freeTypeVariables (TyApplication a b)                        = Set.union (freeTypeVariables a) (freeTypeVariables b)

freePolyTypeVariables :: Ord n => PolyType n -> Set n
freePolyTypeVariables (PolyType vs t) = Set.difference (freeTypeVariables t) vs
