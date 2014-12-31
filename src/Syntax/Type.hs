{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections, TypeSynonymInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Syntax.Type where

import Annotation
import Syntactic

import Data.List
import Data.Maybe
import Data.Monoid

import Data.Foldable
import Data.Traversable
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable

import Control.Applicative
import Control.Monad
import Control.Lens

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Text.PrettyPrint.HughesPJ hiding ((<>), empty)
import Text.PrettyPrint.HughesPJClass hiding ((<>), empty)

-- TypeConstant

data TypeConstant = TyArrow
                  | TyCon -- [(Variance, Kind)] Kind

-- Type

data Type' n e = TyVar' n
               | TyApp' e e
               | TyConst' TypeConstant
               | TyForall' n e
               deriving (Functor, Foldable, Traversable)
type Type n a = Ann (Type' n) a

instance Bifunctor Type' where
  first f (TyVar' x)      = TyVar' (f x)
  first f (TyApp' a b)    = TyApp' a b
  first f (TyConst' c)    = TyConst' c
  first f (TyForall' x t) = TyForall' (f x) t
  second = fmap

instance Bifoldable Type' where
  bifoldMap f g (TyVar' x)      = f x
  bifoldMap f g (TyApp' a b)    = g a <> g b
  bifoldMap f g (TyConst' c)    = mempty
  bifoldMap f g (TyForall' x t) = f x <> g t

instance Bitraversable Type' where
  bitraverse f g (TyVar' x)      = TyVar' <$> f x
  bitraverse f g (TyApp' a b)    = TyApp' <$> g a <*> g b
  bitraverse f g (TyConst' c)    = pure $ TyConst' c
  bitraverse f g (TyForall' x t) = TyForall' <$> f x <*> g t

instance Syntactic Type' where
  syntacticVariables (TyVar' x) = Set.singleton x
  syntacticVariables _          = Set.empty

  syntacticBinders (TyForall' x t) = ( Set.singleton x
                                     , TyForall' x (Set.singleton x, t)
                                     )
  syntacticBinders e               = (Set.empty, fmap (Set.empty,) e)

viewType :: Type n a -> [n]  -> ([n], Type n a, [Type n a])
viewType (TyForall x a) vs = viewType a (x:vs)
viewType (a :$ acc) vs     = (vs, a, acc)

largeType :: Type n a -> Bool
largeType (TyVar _)       = False
largeType (TyApp _ _)     = True
largeType (TyConst _)     = False
largeType (TyForall _ a)  = largeType a

instance Pretty n => Pretty (Type n a) where
  pPrint (viewType ?? [] -> (vs, TyConst TyArrow, [a, b])) =
    hsep
    [ if null vs
      then mempty
      else hsep
           [ text "∀"
           , hsep $ fmap pPrint vs
           , text "."
           ]
    , if largeType a
      then parens $ pPrint a
      else pPrint a
    , text "->"
    , pPrint b
    ]
  pPrint (viewType ?? [] -> (vs, tcon, tas))               =
    hsep
    [ if null vs
      then mempty
      else hsep
           [ text "∀"
           , hsep $ fmap pPrint vs
           , text "."
           ]
    , pPrint tcon
    , hsep
      $ tas
      <&> \t ->
           if largeType t
           then parens $ pPrint t
           else pPrint t
    ]
    

pattern TyVar x      <- Ann _ (TyVar' x)
pattern TyApp f t    <- Ann _ (TyApp' f t)
pattern TyConst c    <- Ann _ (TyConst' c)
pattern TyForall x a <- Ann _ (TyForall' x a)

viewTypeApplication :: Type n a -> [Type n a] -> (Type n a, [Type n a])
viewTypeApplication (TyApp a b) acc = viewTypeApplication a (b : acc)
viewTypeApplication a           acc = (a, acc)

pattern t :$ ts <- (flip viewTypeApplication [] -> (t, ts))

makeTypeApplication :: Type n () -> [Type n ()] -> Type n ()
makeTypeApplication = foldl ((fmap.fmap) ann TyApp')

arrowType :: Type n () -> Type n () -> Type n ()
arrowType a b = makeTypeApplication (ann $ TyConst' TyArrow) [a, b]

-- Variance

data Variance = Variance
                { _covariant     :: Bool
                , _contravariant :: Bool
                }
makeLenses ''Variance

-- Kind

data Kind' e = KStar'
             | KArrow' e e
               deriving (Functor, Foldable, Traversable)
type Kind    = Ann Kind' ()

pattern KStar       = Ann () KStar'
pattern KArrow k k' = Ann () (KArrow' k k')

largeKind :: Kind -> Bool
largeKind KStar        = False
largeKind (KArrow _ _) = True

instance Pretty Kind where
  pPrint KStar         = text "★"
  pPrint (KArrow k k') = hsep
                         [ if largeKind k
                           then parens $ pPrint k
                           else pPrint k
                         , text "->"
                         , pPrint k'
                         ]
