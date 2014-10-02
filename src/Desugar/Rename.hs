module Desugar.Rename where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative

import Data.Foldable (foldrM, foldlM)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Syntax.Expression
import Syntax.Type
import Syntax.Module
import Syntax.Name
import Syntax.Location

data RenameError = UnboundName (QName SyntaxName)

type RenameMonad = ExceptT RenameError (ReaderT (Map (QName SyntaxName) (QName CoreName)) (State Int))

renameVariableName :: SyntaxName -> RenameMonad a -> RenameMonad (CoreName, a)
renameVariableName x m = do
  name <- case x of
    UserName s      -> flip CoreName s   <$> get
    GeneratedName i -> flip CoreName "a" <$> get
  modify (+ 1)
  r <- local (Map.insert (QName [] $ Name VariableName x) (QName [] $ Name VariableName name)) m
  return (name, r)

renameExpression :: Expression SyntaxName -> RenameMonad (Expression CoreName)
renameExpression (Locate loc e) = Locate loc <$> renameExpression' e
  where
    renameExpression' :: Expression' SyntaxName -> RenameMonad (Expression' CoreName)
    renameExpression' (EInteger i)             = return $ EInteger i
    renameExpression' (EChar c)                = return $ EChar c
    renameExpression' (EVariable n)            = do
      n' <- Map.lookup n <$> ask
      case n' of
        Nothing -> throwError $ UnboundName n
        Just x  -> return $ EVariable x
    renameExpression' (EApplication f t)       = do
      f' <- renameExpression f
      t' <- renameExpression t
      return $ EApplication f' t'
    renameExpression' (ELambda x e)            = do
      (x', e') <- renameVariableName x $ renameExpression e
      return $ ELambda x' e'
    renameExpression' (ETuple xs)              = do
      xs' <- renameExpression `mapM` xs
      return $ ETuple xs'
    renameExpression' (EIf c a b)              = do
      c' <- renameExpression c
      a' <- renameExpression a
      b' <- renameExpression b
      return $ EIf c' a' b'
    renameExpression' (ELet bs e)              = do
      (bs', e') <- renameBindings bs (renameExpression e)
      return $ ELet bs' e'

    renameExpression' (EListCase e nil x xs r) = do
      e' <- renameExpression e
      nil' <- renameExpression nil
      (x', (xs', r')) <- renameVariableName x $ renameVariableName xs $ renameExpression r
      return $ EListCase e' nil' x' xs' r'

renameBindings :: BindingMap SyntaxName -> RenameMonad a -> RenameMonad (BindingMap CoreName, a)
renameBindings mp m =
  let keys  = Map.keys mp
      elems = Map.elems mp
    in do
  (ks, es, mv) <- foldr
    (\k acc -> do
      (k', (ks, es, mv)) <- renameVariableName k acc
      return $ (k' : ks, es, mv)
    )
    (do
      mv <- m
      es <- renameExpression `mapM` elems
      return ([], es, mv)
    )
    keys
  return (Map.fromList $ zip ks es, mv)