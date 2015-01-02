{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections, OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Control.Arrow hiding (first, second, (<+>))
import Control.Lens

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map


data Expr' ty n e = EVar' n
                  | EApp' e e
                  | EAbs' n e
                  | ELet' [(n, e)] e
                  | ECase' e [(Pat n, e)]
                  | EAnnot' ty e
                  | EInt' Integer
                  | EChar' Char
                  deriving (Functor, Foldable, Traversable, Show)
type AExpr ty n a = Ann (Expr' ty n) ((), a)
type Expr n a = AExpr (Type n ()) n a

instance Trifunctor Expr' where
  trimap f g h (EVar' x)        = EVar' (g x)
  trimap f g h (EApp' a b)      = EApp' (h a) (h b)
  trimap f g h (EAbs' x a)      = EAbs' (g x) (h a)
  trimap f g h (ELet' bs e)     = ELet' (fmap (bimap g h) bs) (h e)
  trimap f g h (ECase' e cs)    = ECase' (h e) (fmap (over _1 (fixMap g) . over _2 h) cs)
  trimap f g h (EAnnot' ty e)   = EAnnot' (f ty) (h e)
  trimap f g h (EInt' i)        = EInt' i
  trimap f g h (EChar' c)       = EChar' c

instance Trifoldable Expr' where
  trifoldMap f g h (EVar' x)        = g x
  trifoldMap f g h (EApp' a b)      = h a <> h b
  trifoldMap f g h (EAbs' x a)      = g x <> h a
  trifoldMap f g h (ELet' bs e)     = mconcat (fmap (bifoldMap g h) bs) <> h e
  trifoldMap f g h (ECase' e cs)    = h e <> mconcat (fmap (bifoldMap (fixFoldMap g) h) cs)
  trifoldMap f g h (EAnnot' ty e)   = f ty <> h e
  trifoldMap f g h (EInt' _)        = mempty
  trifoldMap f g h (EChar' _)       = mempty
  
instance Tritraversable Expr' where
  tritraverse f g h (EVar' x)        = EVar'   <$> g x
  tritraverse f g h (EApp' a b)      = EApp'   <$> h a <*> h b
  tritraverse f g h (EAbs' x a)      = EAbs'   <$> g x <*> h a
  tritraverse f g h (ELet' bs e)     = ELet'   <$> traverse (bitraverse g h) bs <*> h e
  tritraverse f g h (ECase' e cs)    = ECase'  <$> h e <*> traverse (bitraverse (fixTraverse g) h) cs
  tritraverse f g h (EAnnot' ty e)   = EAnnot' <$> f ty <*> h e
  tritraverse f g h (EInt' i)        = pure $ EInt' i
  tritraverse f g h (EChar' c)       = pure $ EChar' c


eVar :: InductiveAnnotable (Expr' ty n) a => n -> AExpr ty n a
eVar x = ann (EVar' x)
eApp :: InductiveAnnotable (Expr' ty n) a => AExpr ty n a -> AExpr ty n a -> AExpr ty n a
eApp f t = ann (EApp' f t)
eAbs :: InductiveAnnotable (Expr' ty n) a => n -> AExpr ty n a -> AExpr ty n a
eAbs x a = ann (EAbs' x a)
eLet :: InductiveAnnotable (Expr' ty n) a => [(n, AExpr ty n a)] -> AExpr ty n a -> AExpr ty n a
eLet bs e = ann (ELet' bs e)
eCase :: InductiveAnnotable (Expr' ty n) a => AExpr ty n a -> [(Pat n, AExpr ty n a)] -> AExpr ty n a
eCase e cs = ann (ECase' e cs)
eAnnot :: InductiveAnnotable (Expr' ty n) a => ty -> AExpr ty n a -> AExpr ty n a
eAnnot ty e = ann (EAnnot' ty e)
eInt :: InductiveAnnotable (Expr' ty n) a => Integer -> AExpr ty n a
eInt i = ann (EInt' i)
eChar :: InductiveAnnotable (Expr' ty n) a => Char -> AExpr ty n a
eChar c = ann (EChar' c)
  
pattern EVar x        <- Ann _ (EVar' x)
pattern EApp f t      <- Ann _ (EApp' f t)
pattern EAbs x a      <- Ann _ (EAbs' x a)
pattern ELet bs e     <- Ann _ (ELet' bs e)
pattern ECase e cs    <- Ann _ (ECase' e cs)
pattern EAnnot ty e   <- Ann _ (EAnnot' ty e)
pattern EInt i        <- Ann _ (EInt' i)
pattern EChar c       <- Ann _ (EChar' c)


instance Syntactic (Expr' ty) where
  syntacticVariables (EVar' x) = [x]
  syntacticVariables _         = []

  syntacticBinders (EAbs' x e)      = ( [x]
                                      , EAbs' x ([x], e)
                                      )
  syntacticBinders (ELet' bs e)     = let vs = fmap fst bs in
                                       ( vs
                                       , ELet' (fmap (second (vs,)) bs) (vs, e)
                                       )
  syntacticBinders (ECase' e cs)    = ( mconcat (fmap (syntacticFreeVariables . fst) cs)
                                      , ECase'
                                        ([], e)
                                        (fmap (\(p, e) -> (p, (syntacticFreeVariables p, e))) cs)
                                      )
  syntacticBinders e                = ([], fmap ([],) e)

data Pat' n e = PAny'
                { _patternVariables :: [n]
                }
              | PCon'
                { _patternConstructor :: n
                , _patternArguments   :: [e]
                , _patternVariables   :: [n]
                }
              deriving (Functor, Foldable, Traversable, Show)
type Pat n = Ann (Pat' n) ()

makeLenses ''Pat'

instance Bifunctor Pat' where
  first f (PAny' vs)          = PAny' (fmap f vs)
  first f (PCon' con args vs) = PCon' (f con) args (fmap f vs)
  second = fmap
  
instance Bifoldable Pat' where
  bifoldMap f g (PAny' vs) = foldMap f vs
  bifoldMap f g (PCon' con args vs) = f con <> foldMap g args <> foldMap f vs

instance Bitraversable Pat' where
  bitraverse f g (PAny' vs)          = PAny' <$> traverse f vs
  bitraverse f g (PCon' con args vs) = PCon' <$> f con <*> traverse g args <*> traverse f vs

instance Syntactic Pat' where
  syntacticVariables = view patternVariables
  syntacticBinders e = ([], fmap ([],) e)

pattern PAny vs          = Ann () (PAny' vs)
pattern PCon con args vs = Ann () (PCon' con args vs)

type DeclarationMap n e = Map n e

