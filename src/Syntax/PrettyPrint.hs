{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections, OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
module Syntax.PrettyPrint where

import Annotation
import Syntactic

import Syntax.Type
import Syntax.Name
import Syntax.Expression
import Syntax.Module

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
import Control.Arrow hiding (first, second, (<+>))
import Control.Lens

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Text.PrettyPrint.HughesPJ hiding ((<>), empty)
import Text.PrettyPrint.HughesPJClass hiding ((<>), empty)

class Pretty a => PrettyLarge a where
  isLarge :: a -> Bool
  pLarge :: a -> Doc
  pLarge x
    | isLarge x = parens (pPrint x)
    | otherwise = pPrint x

instance Pretty Name where
  pPrint (Name _ (ModuleName []) mn) = text mn
  pPrint (Name _ md mn)              = pPrint md <> char '.' <> text mn

instance Pretty UniqueName where
  pPrint (UniqueName id n) = pPrint n

instance Pretty ModuleName where
  pPrint mn = text (show mn)


instance PrettyLarge Kind where
  isLarge KStar        = False
  isLarge (KArrow _ _) = True

instance Pretty Kind where
  pPrint KStar         = text "★"
  pPrint (KArrow k k') = hsep
                         [ pLarge k
                         , text "->"
                         , pPrint k'
                         ]

instance Pretty n => PrettyLarge (Type n a) where
  isLarge (TyVar _)       = False
  isLarge (TyApp _ _)     = True
  isLarge (TyCon _)       = False
  isLarge (TyForall _ a)  = isLarge a

instance Pretty n => Pretty (Type n a) where
  pPrint (viewType ?? [] -> (vs, tcon, tas)) =
    hsep
    [ if null vs
      then mempty
      else hsep
           [ text "∀"
           , hsep $ fmap pPrint vs
           , text "."
           ]
    , case tcon of
       TyVar x -> pPrint x
       TyCon x -> pPrint x 
    , hsep (pLarge <$> tas)
    ]
 
instance (Pretty ty, Pretty n, Show ty, Show n, Show a, Show b) => PrettyLarge (Ann' (Expr' ty n) a b) where
  isLarge (EVar _)  = False
  isLarge (EInt _)  = False
  isLarge (EChar _) = False
  isLarge _         = True

instance (Pretty ty, Pretty n, Show ty, Show n, Show a, Show b) => Pretty (Ann' (Expr' ty n) a b) where
  pPrint (EVar x)          = pPrint x
  pPrint (EApp f t)        = pPrint f <+> pLarge t
  pPrint (EAbs x e)        = ("λ" <> pPrint x) <+> "->" <+> pPrint e
  pPrint (ELet [(n, t)] e) = "let" <+> pPrint n <+> "=" <+> pPrint t <+> "in" <+> pPrint e
  pPrint (ELet bs e)       = "let"
                             <+> (nest 1 . vcat) (fmap (\(n, t) -> pPrint n <+> "=" <+> pPrint t) bs)
                             $+$ "in" <+> pPrint e
  pPrint (ECase e cs)      = ("case" <+> pPrint e <+> "of")
                         $$ nest 1 (vcat (fmap (\(pat, e) -> pPrint pat <+> "->" <+> pPrint e) cs))
  pPrint (EAnnot ty e)     = pPrint e <+> "::" <+> pPrint ty
  pPrint (EInt x)          = text (show x)
  pPrint (EChar c)         = text (show c)

instance Pretty n => PrettyLarge (Ann' (Pat' n) () ()) where
  isLarge (PAny _)      = False
  isLarge (PCon _ [] _) = False
  isLarge (PCon _ _ _)  = True

instance Pretty n => Pretty (Ann' (Pat' n) () ()) where
  pPrint (PAny [])              = "_"
  pPrint (PAny as)              = foldr1 (\a b -> a <+> "@" <+> b) (pPrint <$> as)
  pPrint (PCon con args [])     = hsep (pPrint con : fmap pLarge args)
  pPrint (PCon con args (a:as)) = pPrint a <> "@" <> parens (pPrint (PCon con args as))

-- instance Pretty n => Pretty (Constraint n a) where
--   -- pPrint (CsUnify a b)    = hsep
--   --                           [ pPrint a
--   --                           , text "≡"
--   --                           , pPrint b
--   --                           ]
--   pPrint (CsInstance a b) = hsep
--                             [ pPrint a
--                             , text "≤"
--                             , pPrint b
--                             ]

instance (Pretty n, Show n) => PrettyLarge (Module n) where
  isLarge _ = True

instance (Pretty n, Show n) => Pretty (Module n) where
  pPrint (Module mn exps inps decls tdecls cdecls idecls) =
    vcat
    [ "module" <+> pPrint mn <+> "where"
    , nest 1 $
      vcat (Map.toList tdecls
            <&> \case
              (n, DataDeclaration vs cons) -> "data" <+> pPrint n <+> hsep (pPrint <$> vs) <+> "="
                                              <+> nest 1 (vcat $ cons <&> \(DataConstructor n tys) ->
                                                                           "|" <+> pPrint n <+> hsep (pPrint <$> tys)
                                                         )
              (n, TypeDeclaration vs ty)   -> "type" <+> pPrint n <+> hsep (pPrint <$> vs) <+> "=" <+> pPrint ty
              (n, PrimitiveTypeDeclaration vs) -> "primitive type" <+> pPrint n <+> hsep (pPrint <$> vs)
           )
    , nest 1 $
      vcat (Map.toList decls
            <&> \(n, e) -> pPrint n <+> "=" <+> pPrint e
           )
    ]
