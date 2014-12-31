{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Syntax.Expression where

import Annotation
import Syntactic

import Syntax.Type

import Data.List
import Data.Maybe
import Data.Monoid

import Data.Foldable
import Data.Traversable
import Data.Bifunctor
import Data.Bifoldable
import Data.Bitraversable
import Data.Trifunctor
import Data.Trifoldable
import Data.Tritraversable

import Control.Applicative
import Control.Monad
import Control.Arrow hiding (first, second)
import Control.Lens

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

data Expr' ty n e = EVar' n
                  | EApp' e e
                  | EAbs' n e
                  | ELet' [(n, e)] e
                  | ECase' e [(Pat n, e)] (Maybe e)
                  | EAnnot' ty e
                  deriving (Functor, Foldable, Traversable)
type Expr ty n a = Ann (Expr' ty n) a

instance Trifunctor Expr' where
  trimap f g h (EVar' x)        = EVar' (g x)
  trimap f g h (EApp' a b)      = EApp' (h a) (h b)
  trimap f g h (EAbs' x a)      = EAbs' (g x) (h a)
  trimap f g h (ELet' bs e)     = ELet' (fmap (bimap g h) bs) (h e)
  trimap f g h (ECase' e cs df) = ECase' (h e) (fmap (over _1 (fmap g) . over _2 h) cs) (fmap h df)
  trimap f g h (EAnnot' ty e)   = EAnnot' (f ty) (h e)

instance Trifoldable Expr' where
  trifoldMap f g h (EVar' x)        = g x
  trifoldMap f g h (EApp' a b)      = h a <> h b
  trifoldMap f g h (EAbs' x a)      = g x <> h a
  trifoldMap f g h (ELet' bs e)     = mconcat (fmap (bifoldMap g h) bs) <> h e
  trifoldMap f g h (ECase' e cs df) = h e <> mconcat (fmap (bifoldMap (foldMap g) h) cs) <> foldMap h df
  trifoldMap f g h (EAnnot' ty e)   = f ty <> h e
  
instance Tritraversable Expr' where
  tritraverse f g h (EVar' x)        = EVar'   <$> g x
  tritraverse f g h (EApp' a b)      = EApp'   <$> h a <*> h b
  tritraverse f g h (EAbs' x a)      = EAbs'   <$> g x <*> h a
  tritraverse f g h (ELet' bs e)     = ELet'   <$> traverse (bitraverse g h) bs <*> h e
  tritraverse f g h (ECase' e cs df) = ECase'  <$> h e <*> traverse (bitraverse (traverse g) h) cs <*> traverse h df
  tritraverse f g h (EAnnot' ty e)   = EAnnot' <$> f ty <*> h e
  
pattern EVar x        <- Ann _ (EVar' x)
pattern EApp f t      <- Ann _ (EApp' f t)
pattern EAbs x a      <- Ann _ (EAbs' x a)
pattern ELet bs e     <- Ann _ (ELet' bs e)
pattern ECase e cs df <- Ann _ (ECase' e cs df)
pattern EAnnot ty e   <- Ann _ (EAnnot' ty e)

instance Syntactic (Expr' ty) where
  syntacticVariables (EVar' x) = Set.singleton x
  syntacticVariables _         = Set.empty

  syntacticBinders (EAbs' x e)      = ( Set.singleton x
                                      , EAbs' x (Set.singleton x, e)
                                      )
  syntacticBinders (ELet' bs e)     = let vs = Set.fromList $ fmap fst bs in
                                       ( vs
                                       , ELet' (fmap (second (vs,)) bs) (vs, e)
                                       )
  syntacticBinders (ECase' e cs df) = ( mconcat (fmap (\(p, e) -> foldMap Set.singleton p) cs)
                                      , ECase'
                                        (Set.empty, e)
                                        (fmap (\(p, e) -> (p, (foldMap Set.singleton p, e))) cs)
                                        (fmap (Set.empty,) df)
                                      )
  syntacticBinders e                = (Set.empty, fmap (Set.empty,) e)

data Pat n = PAny
           | PAs (Pat n)
           | PCon n (Pat n)
           deriving (Functor, Foldable, Traversable)

type DeclarationMap n e = Map n e 
