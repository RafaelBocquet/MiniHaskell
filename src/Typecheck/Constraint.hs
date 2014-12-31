{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections, TypeSynonymInstances #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Typecheck.Constraint where

import Syntax.Type

import Data.Monoid
import Control.Lens

import Text.PrettyPrint.HughesPJ hiding ((<>), empty)
import Text.PrettyPrint.HughesPJClass hiding ((<>), empty)

-- Constaint

data Constraint n a = CsUnify [Type n a]
                    | CsInstance (Type n a) (Type n a)
                    | CsVarInstance n (Type n a)
                    | CsExists n (Constraint n a)
                    | CsAnd [Constraint n a]
                    | CsLet [(n, Type n a)] (Constraint n a)
                    | CsTrue

instance Monoid (Constraint n a) where
  mempty      = CsTrue
  mappend a b = CsAnd [a, b]
  mconcat     = CsAnd

instance Pretty n => Pretty (Constraint n a) where
  -- pPrint (CsUnify a b)    = hsep
  --                           [ pPrint a
  --                           , text "≡"
  --                           , pPrint b
  --                           ]
  pPrint (CsInstance a b) = hsep
                            [ pPrint a
                            , text "≤"
                            , pPrint b
                            ]
csExists :: (n -> Constraint n a) -> Constraint n a
csExists = undefined

csExistsMany :: [b] -> ([(n, b)] -> Constraint n a) -> Constraint n a
csExistsMany xs f = csExistsMany' f (reverse xs)
  where
    csExistsMany' f []       = f []
    csExistsMany' f (x : xs) = csExistsMany' (\ys -> csExists $ \n -> f $ (n, x) : ys) xs
