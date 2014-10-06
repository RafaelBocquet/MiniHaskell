{-# LANGUAGE RankNTypes #-}

module Desugar.Rename where

-- Name resolution and renaming from SyntaxName to CoreName (no duplicate names)

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative

import Data.Foldable (foldrM, foldlM)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe
import Data.Monoid

import Syntax.Expression
import Syntax.Type
import Syntax.Module
import Syntax.Name
import Syntax.Location

data RenameError = UnboundName (QName SyntaxName)
                 | BoundNameOverlap (QName SyntaxName) [QName CoreName]
                 deriving (Show)

data RenameEntry = RenameLocal (QName CoreName)
                 | RenameGlobal [QName CoreName]
                 deriving (Show)

instance Monoid RenameEntry where
  mempty = RenameGlobal []
  RenameLocal a `mappend` _ = RenameLocal a
  _ `mappend` RenameLocal b = RenameLocal b
  RenameGlobal a `mappend` RenameGlobal b = RenameGlobal (a ++ b)

type RenameMap   = Map (QName SyntaxName) RenameEntry

lookupRename :: QName SyntaxName -> RenameMap -> RenameEntry
lookupRename n m = case Map.lookup n m of
  Just x  -> x
  Nothing -> RenameGlobal []

type RenameMonad = ExceptT RenameError (ReaderT RenameMap (State Int))

type Renaming a b     = a -> RenameMonad b
type RenamingIn a b c = a -> RenameMonad c -> RenameMonad (b, c)

runRenameMonad :: RenameMap -> RenameMonad a -> State Int (Either RenameError a)
runRenameMonad env = flip runReaderT env . runExceptT

renameMany :: (forall c. RenamingIn a b c) -> RenamingIn [a] [b] c
renameMany f vs m = do
  foldr
    (\v acc -> do
      (v', (vs, rv)) <- f v acc
      return $ (v' : vs, rv)
    )
    (do
      m' <- m
      return ([], m')
    )
    vs

renameName :: Bool -> RenamingIn (QName SyntaxName) (QName CoreName) a
renameName l (QName md (Name ns x)) m = do
  name <- case x of
    UserName s      -> flip CoreName s   <$> get
    GeneratedName i -> flip CoreName "a" <$> get
  modify (+ 1)
  let cName = (if l then RenameLocal else RenameGlobal . (: [])) $ QName md $ Name ns name
  r <- local (Map.insertWith mappend (QName md $ Name ns x) cName) m
  return (QName md $ Name ns name, r)

renameVariableName :: Bool -> RenamingIn SyntaxName CoreName a
renameVariableName l n m = do
  (QName [] (Name VariableName x), a) <- renameName l (QName [] $ Name VariableName n) m
  return (x, a)

renameConstructorName :: Bool -> RenamingIn SyntaxName CoreName a
renameConstructorName l n m = do
  (QName [] (Name ConstructorName x), a) <- renameName l (QName [] $ Name ConstructorName n) m
  return (x, a)

renameTypeVariableName :: Bool -> RenamingIn SyntaxName CoreName a
renameTypeVariableName l n m = do
  (QName [] (Name TypeVariableName x), a) <- renameName l (QName [] $ Name TypeVariableName n) m
  return (x, a)

renameTypeConstructorName :: Bool -> RenamingIn SyntaxName CoreName a
renameTypeConstructorName l n m = do
  (QName [] (Name TypeConstructorName x), a) <- renameName l (QName [] $ Name TypeConstructorName n) m
  return (x, a)

renameNames :: Bool -> RenamingIn [QName SyntaxName] [QName CoreName] a
renameNames l = renameMany (renameName l)

renameVariableNames :: Bool -> RenamingIn [SyntaxName] [CoreName] a
renameVariableNames l = renameMany (renameVariableName l)

renameConstructorNames :: Bool -> RenamingIn [SyntaxName] [CoreName] a
renameConstructorNames l = renameMany (renameConstructorName l)

renameTypeVariableNames :: Bool -> RenamingIn [SyntaxName] [CoreName] a
renameTypeVariableNames l = renameMany (renameTypeVariableName l)

renameTypeConstructorNames :: Bool -> RenamingIn [SyntaxName] [CoreName] a
renameTypeConstructorNames l = renameMany (renameTypeConstructorName l)

renameExpression :: Renaming (Expression SyntaxName) (Expression CoreName)
renameExpression (Locate loc e) = Locate loc <$> renameExpression' e
  where
    renameExpression' :: Expression' SyntaxName -> RenameMonad (Expression' CoreName)
    renameExpression' (EInteger i)             = return $ EInteger i
    renameExpression' (EChar c)                = return $ EChar c
    renameExpression' (EVariable n)            = do
      n' <- lookupRename n <$> ask
      case n' of
        RenameGlobal []  -> throwError $ UnboundName n
        RenameGlobal [x] -> return $ EVariable x
        RenameGlobal xs  -> throwError $ BoundNameOverlap n xs
        RenameLocal x    -> return $ EVariable x
    renameExpression' (EApplication f t)       = do
      f' <- renameExpression f
      t' <- renameExpression t
      return $ EApplication f' t'
    renameExpression' (ELambda x e)            = do
      (x', e') <- renameVariableName True x $ renameExpression e
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
      (bs', e') <- renameBindings True bs (renameExpression e)
      return $ ELet bs' e'

    renameExpression' (EListCase e nil x xs r) = do
      e' <- renameExpression e
      nil' <- renameExpression nil
      (x', (xs', r')) <- renameVariableName True x $ renameVariableName True xs $ renameExpression r
      return $ EListCase e' nil' x' xs' r'

renameDeclaration :: Renaming (Declaration SyntaxName) (Declaration CoreName)
renameDeclaration (Declaration e)             = Declaration <$> renameExpression e
renameDeclaration (PrimitiveDeclaration prim) = return $ PrimitiveDeclaration prim

renameMonoType :: Renaming (MonoType SyntaxName) (MonoType CoreName)
renameMonoType t = do
    ks <- Map.keysSet <$> ask
    let fv  = Set.toList . Set.filter (\v -> not $ Set.member (QName [] $ Name TypeVariableName v) ks) $ freeTypeVariables t
        tcs = Set.toList . Set.filter (\v -> not $ Set.member v ks)                                    $ typeConstructors t
    (_, (_, t')) <- renameTypeVariableNames True fv $ renameNames True tcs $ renameMonoType' t
    return t'
  where
    renameMonoType' :: Renaming (MonoType SyntaxName) (MonoType CoreName)
    renameMonoType' (TyVariable n) = do
      RenameLocal (QName [] (Name TypeVariableName n')) <- fromJust . Map.lookup (QName [] (Name TypeVariableName n)) <$> ask
      return $ TyVariable n'
    renameMonoType' (TyApplication n ts) = do
      RenameLocal n'  <- fromJust . Map.lookup n <$> ask
      ts' <- renameMonoType' `mapM` ts
      return $ TyApplication n' ts'

renameDataConstructor :: Renaming (DataConstructor SyntaxName) (DataConstructor CoreName)
renameDataConstructor (DataConstructor n ts) = do
  (n', ()) <- renameTypeConstructorName True n (return ())
  ts'      <- renameMonoType `mapM` ts
  return $ DataConstructor n' ts'

renameDataDeclaration :: Renaming (DataDeclaration SyntaxName) (DataDeclaration CoreName)
renameDataDeclaration (DataDeclaration dcs) = DataDeclaration <$> renameDataConstructor `mapM` dcs

renameMap :: Bool -> Renaming a b -> RenamingIn (Map SyntaxName a) (Map CoreName b) c
renameMap l f mp m = do
  let keys  = Map.keys mp
      elems = Map.elems mp
  (ks, (es, mv)) <- renameVariableNames l keys $ do
    es <- f `mapM` elems
    mv <- m
    return (es, mv)
  return (Map.fromList $ zip ks es, mv)

renameBindings :: Bool -> RenamingIn (DeclarationMap SyntaxName) (DeclarationMap CoreName) c
renameBindings l = renameMap l renameDeclaration

--renameDataDeclarations :: RenamingIn (DataDeclarationMap SyntaxName) (DataDeclarationMap CoreName) c
--renameDataDeclarations = renameMap renameDataDeclaration

renameModule :: Module SyntaxName -> RenameMonad (RenameMap, Module CoreName)
renameModule (Module mn is ds bs) = do
  (bs', rMap) <- renameBindings False bs ask
  return $ (Map.mapKeys (\n -> case n of { QName [] n -> QName mn n; _ -> n }) rMap, Module mn is Map.empty bs')