{-# LANGUAGE LambdaCase, ViewPatterns, PatternSynonyms, TupleSections #-}

module Typecheck.Expression where

import Control.Lens

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except

import Data.Monoid

import Annotation
import Syntactic

import Syntax.Name
import Syntax.Type
import Syntax.Expression

import Typecheck.Constraint

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

checkExpression :: Expr UniqueName () -> Type UniqueName () -> Constraint UniqueName ()
checkExpression (EVar x) ty      = x `CsVarInstance` ty
checkExpression (EApp f t) ty    = csExists
                                   $ \tau ->
                                      checkExpression t (ann $ TyVar' tau)
-- checkExpression (EAbs x e) ty    = csExists
--                                    $ \(ann . TyVar' -> tau) ->
--                                       csExists
--                                       $ \(ann . TyVar' -> sigma) ->
--                                          CsLet [(x, tau)] (checkExpression e sigma)
--                                          <> (tau `arrowType` sigma) `CsInstance` ty
checkExpression (ELet bs e) ty    = csExistsMany bs
                                    $ \bs' ->
                                    mconcat
                                    $ checkExpression e ty
                                    : (bs' <&> \(ann . TyVar' -> ty, (_, e)) -> checkExpression e ty)
checkExpression (EAnnot ty' e) ty = checkExpression e ty
                                    <> CsUnify [ty, ty']
