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

import Debug.Trace

data RenameError = UnboundName QSyntaxName
                 | BoundNameOverlap QSyntaxName [QCoreName]
                 | DuplicatePatternVariable SyntaxName
                 deriving (Show)

data RenameEntry = RenameLocal QCoreName
                 | RenameGlobal [QCoreName]
                 deriving (Show)

instance Monoid RenameEntry where
  mempty = RenameGlobal []
  RenameLocal a `mappend` _ = RenameLocal a
  _ `mappend` RenameLocal b = RenameLocal b
  RenameGlobal a `mappend` RenameGlobal b = RenameGlobal (a ++ b)

type RenameMap   = Map QSyntaxName RenameEntry

lookupRename :: QSyntaxName -> RenameMonad QCoreName
lookupRename n = do
  m <- ask
  case Map.lookup n m of
    Just n' -> case n' of
      RenameGlobal []  -> throwError $ UnboundName n
      RenameGlobal [x] -> return $ x
      RenameGlobal xs  -> throwError $ BoundNameOverlap n xs
      RenameLocal x    -> return $ x
    Nothing -> traceShow m $ throwError $ UnboundName n

type RenameMonad = ExceptT RenameError (ReaderT RenameMap (State Int))

type Renaming a b     = a -> RenameMonad b
type RenamingIn a b c = a -> RenameMonad c -> RenameMonad (b, c)

runRenameMonad :: RenameMap -> RenameMonad a -> State Int (Either RenameError a)
runRenameMonad env = flip runReaderT env . runExceptT

renameMany :: (forall c. RenamingIn a b c) -> RenamingIn [a] [b] c
renameMany f vs m =
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

renameName :: RenamingIn QSyntaxName QCoreName a
renameName (QName md ns x) m = do
  name <- case x of
    UserName s      -> flip CoreName s   <$> get
    GeneratedName i -> flip CoreName "a" <$> get
  modify (+ 1)
  r <- if md == []
    then local (Map.insertWith mappend (QName md ns x) (RenameLocal $ QName md ns name)) m
    else local (Map.insertWith mappend (QName md ns x) (RenameGlobal [QName md ns name])
              . Map.insertWith mappend (QName [] ns x) (RenameGlobal [QName md ns name])) m
  return (QName md ns name, r)

renameNameInNamespace :: NameSpace -> ModuleName -> RenamingIn SyntaxName CoreName a
renameNameInNamespace ns md n m = do
  (QName _ _ x, a) <- renameName (QName md ns n) m
  return (x, a)

renameVariableName :: ModuleName -> RenamingIn SyntaxName CoreName a
renameVariableName = renameNameInNamespace VariableName

renameConstructorName :: ModuleName -> RenamingIn SyntaxName CoreName a
renameConstructorName = renameNameInNamespace ConstructorName

renameTypeVariableName :: ModuleName -> RenamingIn SyntaxName CoreName a
renameTypeVariableName = renameNameInNamespace TypeVariableName

renameTypeConstructorName :: ModuleName -> RenamingIn SyntaxName CoreName a
renameTypeConstructorName = renameNameInNamespace TypeConstructorName

renameNames :: RenamingIn [QSyntaxName] [QCoreName] a
renameNames = renameMany renameName

renameVariableNames :: ModuleName -> RenamingIn [SyntaxName] [CoreName] a
renameVariableNames md = renameMany (renameVariableName md)

renameConstructorNames :: ModuleName -> RenamingIn [SyntaxName] [CoreName] a
renameConstructorNames md = renameMany (renameConstructorName md)

renameTypeVariableNames :: ModuleName -> RenamingIn [SyntaxName] [CoreName] a
renameTypeVariableNames md = renameMany (renameTypeVariableName md)

renameTypeConstructorNames :: ModuleName -> RenamingIn [SyntaxName] [CoreName] a
renameTypeConstructorNames md = renameMany (renameTypeConstructorName md)

renamePattern :: RenamingIn (Pattern SyntaxName) (Pattern CoreName) c
renamePattern pat m = renamePattern' Set.empty pat (\_ -> m)
  where
    renamePattern' :: Set SyntaxName -> Pattern SyntaxName -> (Set SyntaxName -> RenameMonad c) -> RenameMonad (Pattern CoreName, c)
    renamePattern' st (Pattern pat vs) m
      | not . Set.null $ Set.intersection (Set.fromList vs) st = throwError $ DuplicatePatternVariable (Set.findMin (Set.intersection (Set.fromList vs) st))
      | otherwise                                              = do
        (vs', (pat', c)) <- renameMany (renameVariableName []) vs $ renamePattern'' (Set.union (Set.fromList vs) st) pat m
        return (Pattern pat' vs', c)
        
    renamePattern'' :: Set SyntaxName -> Pattern' SyntaxName -> (Set SyntaxName -> RenameMonad c) -> RenameMonad (Pattern' CoreName, c)
    renamePattern'' st PWildcard m            = do
      c <- m st
      return (PWildcard, c) 
    renamePattern'' st (PConstructor con pats) m = do
      con' <- lookupRename con
      (pats', c) <- foldr
        (\v acc -> \st' -> do
          (v', (vs, rv)) <- renamePattern' st' v acc
          return $ (v' : vs, rv)
        )
        (\st' -> do
          m' <- m st'
          return ([], m')
        )
        pats
        st
      return (PConstructor con' pats', c)
    renamePattern'' st (PLiteralInt i) m      = do
      c <- m st
      return (PLiteralInt i, c) 
    renamePattern'' st (PLiteralChar ch) m     = do
      c <- m st
      return (PLiteralChar ch, c)


renameExpression :: Renaming (Expression SyntaxName) (Expression CoreName)
renameExpression (Locate loc e) = Locate loc <$> renameExpression' e
  where
    renameExpression' :: Expression' SyntaxName -> RenameMonad (Expression' CoreName)
    renameExpression' (EInteger i)             = return $ EInteger i
    renameExpression' (EChar c)                = return $ EChar c
    renameExpression' (EVariable n)            = do
      EVariable <$> lookupRename n
    renameExpression' (EApplication f t)       = do
      f' <- renameExpression f
      t' <- renameExpression t
      return $ EApplication f' t'
    renameExpression' (ELambda x e)            = do
      (x', e') <- renameVariableName [] x $ renameExpression e
      return $ ELambda x' e'
    renameExpression' (ETuple xs)              = do
      xs' <- renameExpression `mapM` xs
      return $ ETuple xs'
    --renameExpression' (EIf c a b)              = do
    --  c' <- renameExpression c
    --  a' <- renameExpression a
    --  b' <- renameExpression b
    --  return $ EIf c' a' b'
    renameExpression' (ELet bs e)              = do
      (bs', e') <- renameDeclarations [] bs (renameExpression e)
      return $ ELet bs' e'
    renameExpression' (ECase e pats)           = do
      e' <- renameExpression e
      pats' <- forM pats $ \(pat, pate) -> renamePattern pat $ renameExpression pate
      return $ ECase e' pats'


    --renameExpression' (EListCase e nil x xs r) = do
    --  e' <- renameExpression e
    --  nil' <- renameExpression nil
    --  (x', (xs', r')) <- renameVariableName [] x $ renameVariableName [] xs $ renameExpression r
    --  return $ EListCase e' nil' x' xs' r'

renameDeclaration :: Renaming (Declaration SyntaxName) (Declaration CoreName)
renameDeclaration (Declaration Nothing e)     = Declaration Nothing <$> renameExpression e
renameDeclaration (Declaration (Just t) e)    = do
  t' <- renamePolyType t
  e' <- renameExpression e
  return $ Declaration (Just t') e'
renameDeclaration (PrimitiveDeclaration prim) = return $ PrimitiveDeclaration prim

renamePolyType :: Renaming (MonoType SyntaxName) (MonoType CoreName)
renamePolyType t = do
    ks <- Map.keysSet <$> ask
    let fv = Set.toList . Set.filter (\v -> not $ Set.member (QName [] TypeVariableName v) ks) $ freeTypeVariables t
    (_, t') <- renameTypeVariableNames [] fv $ renameMonoType t
    return t'

renameMonoType :: Renaming (MonoType SyntaxName) (MonoType CoreName)
renameMonoType (TyVariable n)      = do
  x <- lookupRename (QName [] TypeVariableName n)
  case x of
    QName [] TypeVariableName v -> return $ TyVariable v
    _                           -> throwError $ UnboundName (QName [] TypeVariableName n)
renameMonoType (TyConstant n)      = do
  TyConstant <$> lookupRename n
renameMonoType (TyApplication a b) = liftM2 TyApplication (renameMonoType a) (renameMonoType b)
renameMonoType TyArrow             = return TyArrow

renameDataConstructor :: ModuleName -> RenamingIn (DataConstructor SyntaxName) (DataConstructor CoreName) c
renameDataConstructor md (DataConstructor n ts) m = do
  (n', (ts', c')) <- renameConstructorName md n $ do
    ts' <- renameMonoType `mapM` ts
    c   <- m
    return (ts', c)
  return (DataConstructor n' ts', c')

renameTypeDeclaration :: ModuleName -> RenamingIn (TypeDeclaration SyntaxName) (TypeDeclaration CoreName) c
renameTypeDeclaration md (DataDeclaration tvs dcs) m           = do
  (tvs', (dcs', c)) <- renameTypeVariableNames [] tvs $ renameMany (renameDataConstructor md) dcs m
  return (DataDeclaration tvs' dcs', c)
renameTypeDeclaration md (PrimitiveDataDeclaration prim) m = do
  c <- m
  return (PrimitiveDataDeclaration prim, c)

renameMapIn :: ModuleName -> NameSpace -> (forall c. RenamingIn a b c) -> RenamingIn (Map SyntaxName a) (Map CoreName b) c
renameMapIn md ns f mp m = do
  let keys  = Map.keys mp
      elems = Map.elems mp
  (ks, (es, mv)) <- renameMany (renameNameInNamespace ns md) keys $ renameMany f elems m
  return (Map.fromList $ zip ks es, mv)

renameMap :: ModuleName -> NameSpace -> Renaming a b -> RenamingIn (Map SyntaxName a) (Map CoreName b) c
renameMap md ns f mp m = do
  let keys  = Map.keys mp
      elems = Map.elems mp
  (ks, (es, mv)) <- renameMany (renameNameInNamespace ns md) keys $ do
    es <- f `mapM` elems
    mv <- m
    return (es, mv)
  return (Map.fromList $ zip ks es, mv)

renameDeclarations :: ModuleName -> RenamingIn (DeclarationMap SyntaxName) (DeclarationMap CoreName) c
renameDeclarations md = renameMap md VariableName renameDeclaration

renameTypeDeclarations :: ModuleName -> RenamingIn (TypeDeclarationMap SyntaxName) (TypeDeclarationMap CoreName) c
renameTypeDeclarations md = renameMapIn md TypeConstructorName (renameTypeDeclaration md)

renameModule :: Module SyntaxName -> RenameMonad (RenameMap, Module CoreName)
renameModule (Module mn is ds bs) = do
  (ds', (bs', rMap)) <- renameTypeDeclarations mn ds $ renameDeclarations mn bs $ ask
  return $ (Map.filterWithKey (\n _ -> case n of { QName mn' _ _ | mn == mn' -> True; _ -> False }) rMap, Module mn is ds' bs')
