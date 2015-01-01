{-# LANGUAGE TemplateHaskell #-}

module Parsing.Bindings where

import Annotation

import Syntax.Expression
import Syntax.Type
import Syntax.Name
import Syntax.Module

import Parsing.Location
import Parsing.Monad
import Parsing.Util

import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Lens

import Data.Maybe
import Data.Monoid
import Data.Bifunctor

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

import Debug.Trace

-- Bindings

data Bindings = Bindings
                { _bindingsTypes   :: Map Name (Type Name ())
                , _bindingsValues  :: Map Name [([Pat Name], Expr Name ())]
                , _bindingsCurrent :: Maybe Name
                , _bindingsBound   :: Set Name
                }

makeLenses ''Bindings

emptyBindings :: Bindings
emptyBindings = Bindings Map.empty Map.empty Nothing Set.empty

addSignature :: Name -> Type Name () -> Bindings -> ParseMonad Bindings
addSignature n ty bs
  | Map.member n (bs ^. bindingsTypes) = throwError ParseError
  | otherwise                          = return $ bs
                                         & bindingsTypes %~ Map.insert n ty

addDeclaration :: Name -> [Pat Name] -> Expr Name () -> Bindings -> ParseMonad Bindings
addDeclaration n pat e bs
  | maybe True ((/=) n) (bs ^. bindingsCurrent) = bs
                                                   & bindingsBound %~ maybe id Set.insert (bs ^. bindingsCurrent)
                                                   & bindingsCurrent .~ Just n
                                                   & bindingsValues %~ Map.insert n [(pat, e)]
                                                   & return
  | Set.member n (bs ^. bindingsBound)          = throwError ParseError
  | otherwise                                   = bs
                                                  & bindingsValues %~ Map.adjust ((pat, e) :) n
                                                  & return

addPattern :: Pat Name -> Expr Name () -> Bindings -> ParseMonad Bindings
addPattern pat e bs
  | isJust (bs ^. bindingsCurrent) = addPattern pat e
                                     $ bs & bindingsCurrent .~ Nothing
--  | not (Set.null (Set.intersection ))
  | otherwise                      = error "unimplemented : pattern case : pat = e => do { a <- fresh; a = e; forM (vars pat) $ v = case a of pat -> v }"

makeDeclarationMap :: Bindings -> ParseMonad (DeclarationMap Name (Expr Name ()))
makeDeclarationMap (Bindings ty vs _ _)
  | not (Set.null (Map.keysSet ty Set.\\ Map.keysSet vs)) = throwError ParseError
  | otherwise                                             =
      vs
        & mapM makeDeclaration
        <&> Map.mapWithKey (\n -> case Map.lookup n ty of
                                   Nothing -> id
                                   Just ty -> eAnnot ty
                           )
                                                            

makeDeclaration :: [([Pat Name], Expr Name ())] -> ParseMonad (Expr Name ())
makeDeclaration [] = error "ICE"
makeDeclaration cs = do
  let arity = length . fst . head $ cs
  when (not . all ((==) arity) . fmap (length . fst) $ cs)
    $ throwError ParseError
  let vs    = localVar . ('_':) . show <$> [1..arity]
      tuple = foldl eApp (eVar (tupleName arity)) (eVar <$> vs)
  return
    $ foldl (flip eAbs)
    ?? vs
    $ eCase tuple (fmap (first (PCon (tupleName arity) ?? [])) cs)
