{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections, TypeSynonymInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Syntax.Type where

import Annotation
import Syntactic

import Syntax.Name

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

-- Type

data Type' n e = TyVar' n
               | TyApp' e e
               | TyCon' n
               | TyForall' n e
               deriving (Functor, Foldable, Traversable, Show)
type Type n a = Ann (Type' n) ((), a)

instance Bifunctor Type' where
  first f (TyVar' x)      = TyVar' (f x)
  first f (TyApp' a b)    = TyApp' a b
  first f (TyCon' n)      = TyCon' (f n)
  first f (TyForall' x t) = TyForall' (f x) t
  second = fmap

instance Bifoldable Type' where
  bifoldMap f g (TyVar' x)      = f x
  bifoldMap f g (TyApp' a b)    = g a <> g b
  bifoldMap f g (TyCon' n)      = f n
  bifoldMap f g (TyForall' x t) = f x <> g t

instance Bitraversable Type' where
  bitraverse f g (TyVar' x)      = TyVar' <$> f x
  bitraverse f g (TyApp' a b)    = TyApp' <$> g a <*> g b
  bitraverse f g (TyCon' n)      = TyCon' <$> f n
  bitraverse f g (TyForall' x t) = TyForall' <$> f x <*> g t

instance Syntactic Type' where
  syntacticVariables (TyVar' x) = [x]
  syntacticVariables (TyCon' c) = [c]
  syntacticVariables _          = []

  syntacticBinders (TyForall' x t) = ( [x]
                                     , TyForall' x ([x], t)
                                     )
  syntacticBinders e               = ([], fmap ([],) e)

viewType :: Type n a -> [n]  -> ([n], Type n a, [Type n a])
viewType (TyForall x a) vs = viewType a (x:vs)
viewType (a :$ acc) vs     = (vs, a, acc)   

tyVar :: InductiveAnnotable (Type' n) a => n -> Type n a
tyVar x = ann (TyVar' x)
tyApp :: InductiveAnnotable (Type' n) a => Type n a -> Type n a -> Type n a
tyApp f t = ann (TyApp' f t)
tyCon :: InductiveAnnotable (Type' n) a => n -> Type n a
tyCon c = ann (TyCon' c)
tyForall :: InductiveAnnotable (Type' n) a => n -> Type n a -> Type n a
tyForall x a = ann (TyForall' x a)

pattern TyVar x      <- Ann _ (TyVar' x)
pattern TyApp f t    <- Ann _ (TyApp' f t)
pattern TyCon c      <- Ann _ (TyCon' c)
pattern TyForall x a <- Ann _ (TyForall' x a)

viewTypeApplication :: Type n a -> [Type n a] -> (Type n a, [Type n a])
viewTypeApplication (TyApp a b) acc = viewTypeApplication a (b : acc)
viewTypeApplication a           acc = (a, acc)

pattern t :$ ts <- (flip viewTypeApplication [] -> (t, ts))

makeTypeApplication :: InductiveAnnotable (Type' n) a => Type n a -> [Type n a] -> Type n a
makeTypeApplication = foldl ((fmap.fmap) ann TyApp')

arrowType :: InductiveAnnotable (Type' Name) a => Type Name a -> Type Name a -> Type Name a
arrowType a b = makeTypeApplication (ann $ TyCon' $ Name NsTyCon (ModuleName ["Primitive"]) "->") [a, b]

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
