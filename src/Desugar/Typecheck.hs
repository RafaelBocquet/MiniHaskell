{-# LANGUAGE FlexibleContexts #-}

module Desugar.Typecheck where

import Prelude hiding (forM, mapM)

import Control.Monad.Except hiding (forM, mapM)
import Control.Monad.State hiding (forM, mapM)
import Control.Monad.Reader hiding (forM, mapM)
import Control.Applicative
import Control.Arrow ((***))

import Data.Maybe
import Data.Monoid
import Data.Foldable (foldlM, foldrM)
import Data.Traversable

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Syntax.Expression
import Syntax.Type
import Syntax.Module
import Syntax.Name
import Syntax.Location

import Desugar.Unify
import Desugar.Rename

import qualified Core.Module as C
import qualified Core.Expression as C

import Debug.Trace

data Environment = Environment
  { environmentTypeMap              :: Map QCoreName (PolyType CoreName) -- Type of Variables
  , environmentKindMap              :: Map QCoreName (Kind CoreName)     -- Kind of Types
  , environmentRenaming             :: RenameMap
  , environmentInstances            :: Map (QCoreName, CoreName) C.Expression
  , environmentInstanceConstructors :: Map (QCoreName, QCoreName) C.TypeClassExpression
  }

data TypecheckError = UnboundVariable QCoreName
                    | UnboundConstructor QCoreName
                    | TCUnifyError UnifyError
                    | InTypechecking (Expression CoreName) TypecheckError
                    | CantMatchPatternTypes QCoreName QCoreName
                    | UnknownPrimitive String
                    | UnknownError

instance Show TypecheckError where
  show (InTypechecking e err)      = "When typechecking\n\t" ++ show e ++ "\n : \n" ++ show err
  show (TCUnifyError err)          = show err
  show (UnboundVariable v)         = "Unbound variable " ++ show v
  show (UnboundConstructor v)      = "Unbound data constructor " ++ show v
  show (CantMatchPatternTypes a b) = "Cant match pattern types " ++ show a ++ " and " ++ show b
  show (UnknownPrimitive p)        = "Unknown primitive : " ++ show p
  show (UnknownError)              = "Unknown / Not yet implemented error"

type TypecheckMonad = ReaderT Environment (ExceptT TypecheckError (StateT Int UnifyMonad))

generateName :: TypecheckMonad CoreName
generateName = do
  n <- get
  modify (+ 1)
  return $ CoreName n "a"

runTypecheckMonad :: Environment -> TypecheckMonad a -> State Int (Either TypecheckError a)
runTypecheckMonad env m = do
  c     <- get
  let v = runUnifyMonad . flip runStateT c . runExceptT . flip runReaderT env $ m
  case v of
    Left e             -> return (Left $ TCUnifyError e)
    Right (Left e, _)  -> return (Left e)
    Right (Right r, c) -> do
      put c
      return (Right r)

liftUnify :: UnifyMonad a -> TypecheckMonad a
liftUnify m = do
  r <- lift.lift.lift $ (Right <$> m) `catchError` (\e -> return (Left e))
  case r of
    Right r -> return r
    Left e -> throwError (TCUnifyError e)

localBind :: CoreName -> PolyType CoreName -> TypecheckMonad a -> TypecheckMonad a
localBind x t = local $ \s -> s { environmentTypeMap = Map.insert (QName [] VariableName x) t (environmentTypeMap s) }

localBindMany :: Map CoreName (PolyType CoreName) -> TypecheckMonad a -> TypecheckMonad a
localBindMany bs = local $ \s -> s { environmentTypeMap = Map.union (Map.mapKeys (QName [] VariableName) bs) (environmentTypeMap s) }

globalBindMany :: Map QCoreName (PolyType CoreName) -> TypecheckMonad a -> TypecheckMonad a
globalBindMany bs = local $ \s -> s { environmentTypeMap = Map.union bs (environmentTypeMap s) }

bindInstances :: Map (QCoreName, CoreName) C.Expression -> TypecheckMonad a -> TypecheckMonad a
bindInstances is = local $ \s -> s { environmentInstances = Map.union is (environmentInstances s) }

bindInstanceConstructors :: Map (QCoreName, QCoreName) C.TypeClassExpression -> TypecheckMonad a -> TypecheckMonad a
bindInstanceConstructors is = local $ \s -> s { environmentInstanceConstructors = Map.union is (environmentInstanceConstructors s) }

getPrimitive :: NameSpace -> String -> TypecheckMonad QCoreName
getPrimitive ns s = do
  p <- Map.lookup (QName ["Primitive"] ns (UserName s)) . environmentRenaming <$> ask
  case p of
    Just (RenameGlobal [p]) -> return p
    _                       -> throwError $ UnknownPrimitive s

getBase :: NameSpace -> String -> TypecheckMonad QCoreName
getBase ns s = do
  p <- Map.lookup (QName ["Base"] ns (UserName s)) . environmentRenaming <$> ask
  case p of
    Just (RenameGlobal [p]) -> return p
    _                       -> throwError $ UnknownPrimitive s

instantiateType :: PolyType CoreName -> TypecheckMonad (MonoType CoreName, [(QCoreName, MonoType CoreName)])
instantiateType (PolyType as t) = do
    (subst, cs') <- fmap (Map.fromList *** concat) $ fmap unzip $ forM (Map.toList as) $ \(a, cs) -> do
      n <- generateName
      liftUnify $ addConstraints (TyVariable n) cs
      return ((a, n), fmap (flip (,) (TyVariable n)) (Set.toList cs))
    let applySubstitution (TyVariable v)      = case Map.lookup v subst of
          Just a  -> TyVariable a
          Nothing -> TyVariable v
        applySubstitution (TyApplication a b) = TyApplication (applySubstitution a) (applySubstitution b)
        applySubstitution (TyConstant c)      = TyConstant c
        applySubstitution TyArrow             = TyArrow
    ty <- fmap applySubstitution $ liftUnify $ substituteType t
    return (ty, cs')

environmentVariables :: TypecheckMonad (Set CoreName)
environmentVariables = Set.unions . (freePolyTypeVariables <$>) . Map.elems . environmentTypeMap <$> ask

data PatternList = PatternListExpression (Expression CoreName)
                 | PatternListMap (Map (Pattern CoreName) ([Pattern CoreName], PatternList))

patternListInsert :: ([Pattern CoreName], Expression CoreName) -> PatternList -> PatternList
patternListInsert (pat:pats, e) (PatternListMap mp) = 
  PatternListMap $ Map.alter (Just . (\(pats', pl) -> (pat:pats', patternListInsert (pats, e) pl)) . maybe ([], PatternListMap Map.empty) id) (patternSkeleton pat) mp 
patternListInsert ([], e) _                         = PatternListExpression e

unifyPatterns :: [Pattern CoreName] -> Pattern CoreName
unifyPatterns pats =
  let vs    = concat (fmap (\(Pattern _ vs) -> vs) pats)
      pats' = fmap (\(Pattern p _)          -> p) pats
      pat'  = unifyPatterns' pats'
  in Pattern pat' vs

unifyPatterns' :: [Pattern' CoreName] -> Pattern' CoreName
unifyPatterns' (PWildcard : _)              = PWildcard
unifyPatterns' pats@(PConstructor n ps : _) =
  let ps' = fmap unifyPatterns $ foldr (\(PConstructor _ ps) acc ->
                                         zipWith (:) ps acc
                                       ) (const [] <$> ps) pats in
  PConstructor n ps'
unifyPatterns' ((PLiteralInt i) : _)       = PLiteralInt i
unifyPatterns' ((PLiteralChar c) : _)      = PLiteralChar c
  

reducePatternList :: Maybe (Expression CoreName) -> [CoreName] -> PatternList -> Expression CoreName
reducePatternList df [] (PatternListExpression e) = e
reducePatternList df (v:vs) (PatternListMap mp)   =
  Locate noLocation $ ECase (Locate noLocation $ EVariable (QName [] VariableName v))
  $ fmap
  (\(_, (pats, pl)) ->
    let e = reducePatternList df vs pl in
    (unifyPatterns pats, e)
  )
  (Map.toList mp)

typecheckPatterns :: Expression CoreName -> [(Pattern CoreName, Expression CoreName)] -> TypecheckMonad C.TypeClassExpression
typecheckPatterns epat pats = do
  pTy <- foldlM patternGroupCombine C.NoPatternGroupType =<< (\(Pattern p _) -> patternGroupType p) `mapM` (fst <$> pats)
  case pTy of
    C.NoPatternGroupType      ->
      typecheckPatternGroupTypeNone pats
    --C.IntPatternGroupType     ->
    --  foldlM
    --    (\(C.PInt ics df) (pat, e) -> do
    --      e' <- typecheckExpression e
    --      case pat of
    --        PLiteralInt i -> return $ C.PInt (Map.insertWith (flip const) i e' ics) df
    --        PWildcard     -> return $ C.PInt ics (maybe (Just e') Just df)
    --    )
    --    (C.PInt Map.empty Nothing)
    --    pats
    C.CharPatternGroupType    -> throwError UnknownError
    C.DataPatternGroupType dc -> typecheckPatternGroupTypeData dc pats
  where
    patternGroupType :: Pattern' CoreName -> TypecheckMonad C.PatternGroupType
    patternGroupType PWildcard               = return $ C.NoPatternGroupType
    patternGroupType (PConstructor con pats) = do
      t <- Map.lookup con . environmentTypeMap <$> ask
      case t of
        Nothing              -> throwError $ UnboundConstructor con
        Just (PolyType _ ty) -> C.DataPatternGroupType <$> constructorDataType ty
    patternGroupType (PLiteralInt _)     = return $ C.IntPatternGroupType
    patternGroupType (PLiteralChar _)    = return $ C.CharPatternGroupType

    patternGroupCombine C.NoPatternGroupType p                                               = return p
    patternGroupCombine (C.DataPatternGroupType dc) C.NoPatternGroupType                     = return $ C.DataPatternGroupType dc
    patternGroupCombine (C.DataPatternGroupType dc) (C.DataPatternGroupType dc') | dc == dc'  = return $ C.DataPatternGroupType dc
    patternGroupCombine (C.DataPatternGroupType dc) (C.DataPatternGroupType dc') | otherwise = throwError $ CantMatchPatternTypes dc dc'
    patternGroupCombine (C.DataPatternGroupType dc) _                                        = throwError UnknownError

    constructorDataType (TyApplication (TyApplication TyArrow a) b) = constructorDataType b
    constructorDataType (TyConstant a)                              = return a
    constructorDataType (TyApplication a b)                         = constructorDataType a
    constructorDataType _                                           = throwError UnknownError

    constructorResultType (TyApplication (TyApplication TyArrow a) b) = constructorResultType b
    constructorResultType a                                           = return a

    constructorArguments (TyApplication (TyApplication TyArrow a) b)  = (a :) <$> constructorArguments b
    constructorArguments _                                            = return []

    typecheckPatternGroupTypeNone pats = case pats of
      (Pattern PWildcard [], e):[]   -> do
        epat' <- typecheckExpression epat
        e'    <- typecheckExpression e
        return $ e'
      (Pattern PWildcard vs, e):[]   -> do
        epat' <- typecheckExpression epat
        e'    <- localBindMany (Map.fromList $ (\v -> (v, PolyType Map.empty (C.typeclassExpressionType epat'))) <$> vs) $ typecheckExpression e
        return $ C.Expression
              (C.typeclassExpressionType e', [])
              (C.ELet (Map.fromList
                        $ ( head vs
                          , ( PolyType Map.empty (C.typeclassExpressionType epat')
                            , epat'
                            )
                          )
                        : fmap (\v -> ( v
                                      , ( PolyType Map.empty (C.typeclassExpressionType epat')
                                        , C.Expression (C.typeclassExpressionType epat', []) (C.EVariable (QName [] VariableName (head vs)))
                                        )
                                      )
                               ) (tail vs)
                      )
                $ e'
              )
      _ -> error (show pats)

    typecheckPatternGroupTypeData dc pats = do
      epat' <- typecheckExpression epat
      let df = foldl
            (\df (pat, e) -> case pat of
              Pattern PWildcard vs -> Just $ maybe e id df
              _                    -> df
            )
            Nothing
            pats
          cases = foldl
            (\mp (pat, e) -> case pat of
              Pattern (PConstructor con pats) vs -> Map.alter (Just . patternListInsert (pats, e) . maybe (PatternListMap Map.empty) id) con mp
              _                                  -> mp
            )
            Map.empty
            pats
          vs = concat $ fmap (\(Pattern _ pvs, _) -> pvs) pats
      (df', dfvar, sigma, cases) <- localBindMany (Map.fromList $ (\v -> (v, PolyType Map.empty (C.typeclassExpressionType epat'))) <$> vs) $ do
        df'   <- maybe (return Nothing) (fmap Just . typecheckExpression) df
        dfvar <- maybe (return Nothing) (const $ fmap Just generateName) df
        sigma <- TyVariable <$> generateName
        cases <- fmap Map.fromList $ maybe id (\v -> localBind v (PolyType Map.empty sigma)) dfvar
                 $ forM (Map.toList cases) $ \(con, patList) -> do
                   (conty, []) <- instantiateType =<< fromJust . Map.lookup con . environmentTypeMap <$> ask
                   conargs     <- constructorArguments conty
                   conret      <- constructorResultType conty
                   liftUnify $ unifyType conret (C.typeclassExpressionType epat')
                   vars        <- forM conargs $ const generateName
                   ecase       <- localBindMany (Map.fromList $ zip vars (PolyType Map.empty <$> conargs))
                              $ typecheckExpression (reducePatternList (Locate noLocation . EVariable . QName [] VariableName <$> dfvar) vars patList)
                   liftUnify $ unifyType (C.typeclassExpressionType ecase) sigma
                   return (con, (vars, ecase))
        return (df', dfvar, sigma, cases)
      return
        $ maybe id (\v -> C.Expression (sigma, []) . C.ELet (Map.singleton v (PolyType Map.empty (C.typeclassExpressionType (fromJust df')), fromJust df'))) dfvar
          $ (if null vs
             then id
             else C.Expression (sigma, []) . (C.ELet (Map.fromList
                                                      $ ( head vs
                                                        , ( PolyType Map.empty (C.typeclassExpressionType epat')
                                                          , epat'
                                                          )
                                                        )
                                                      : fmap (\v -> ( v
                                                                   , ( PolyType Map.empty (C.typeclassExpressionType epat')
                                                                     , C.Expression (C.typeclassExpressionType epat', []) (C.EVariable (QName [] VariableName (head vs)))
                                                                     )
                                                                   )
                                                             ) (tail vs)
                                                     )))
          $ C.Expression (sigma, []) $ C.ECase epat' $ C.PData cases (C.Expression (sigma, []) . C.EVariable . QName [] VariableName <$> dfvar)

typecheckExpression :: Expression CoreName -> TypecheckMonad C.TypeClassExpression
typecheckExpression e = typecheckExpression' (delocate e) `catchError` (\err -> throwError $ InTypechecking e err)
  where
    typecheckExpression' (EInteger i)                                 = do
      tyInt <- TyConstant <$> getBase TypeConstructorName "Int"
      return $ C.Expression (tyInt, []) (C.EInteger i)
    typecheckExpression' (EChar c)                                    = do
      tyChar <- TyConstant <$> getBase TypeConstructorName "Char"
      return $ C.Expression (tyChar, []) (C.EChar c)
    typecheckExpression' (EVariable x) = do
      emap <- environmentTypeMap <$> ask
      t <- Map.lookup x . environmentTypeMap <$> ask
      case t of
        Nothing -> throwError $ UnboundVariable x
        Just t  -> do
          ty <- instantiateType t
          return $ C.Expression ty (C.EVariable x)
    typecheckExpression' (EApplication f t)                           = do
      fTy   <- typecheckExpression f
      tTy   <- typecheckExpression t
      tau   <- generateName
      sigma <- generateName
      liftUnify $ unifyType (C.typeclassExpressionType tTy) (TyVariable tau)
      liftUnify $ unifyType (C.typeclassExpressionType fTy) (makeTypeApplication TyArrow [TyVariable tau, TyVariable sigma])
      return $ C.Expression (TyVariable sigma, []) (C.EApplication fTy tTy)
    typecheckExpression' (ELambda x e)                                = do
      tau     <- generateName
      eTy     <- localBind x (PolyType Map.empty (TyVariable tau)) $ typecheckExpression e
      return $ C.Expression (makeTypeApplication TyArrow [TyVariable tau, C.typeclassExpressionType eTy], []) (C.ELambda x eTy)
    typecheckExpression' (ELet bs e)                                  = do
      bts <- typecheckBindings [] bs
      eTy <- localBindMany (Map.map fst bts) $ typecheckExpression e
      return $ C.Expression (C.typeclassExpressionType eTy, []) (C.ELet (Map.map (\(ty, C.Declaration e) -> (ty, e)) bts) eTy)
    typecheckExpression' (ECase e pats)                               = typecheckPatterns e pats


typecheckDeclaration :: Declaration CoreName -> TypecheckMonad C.TypeClassDeclaration
typecheckDeclaration (Declaration Nothing e)  = C.Declaration <$> typecheckExpression e
typecheckDeclaration (Declaration (Just t) e) = do
  e' <- typecheckExpression e
  (t', []) <- instantiateType $ PolyType (Map.fromSet (const Set.empty) $ freeTypeVariables t) t
  liftUnify $ unifyType t' (C.typeclassExpressionType e')
  return $ C.Declaration e'
typecheckDeclaration (PrimitiveDeclaration prim) = return $ C.PrimitiveDeclaration prim

primitiveType :: PrimitiveDeclaration -> TypecheckMonad (MonoType CoreName)
primitiveType p 
  | p `elem` [ PrimitiveIntAdd, PrimitiveIntSub, PrimitiveIntMul, PrimitiveIntDiv, PrimitiveIntRem ]
      = do
    tyIntP  <- TyConstant <$> getPrimitive TypeConstructorName "Int_prim"
    tyInt  <- TyConstant <$> getBase TypeConstructorName "Int"
    return $ makeTypeApplication TyArrow [tyIntP, makeTypeApplication TyArrow [tyIntP, tyInt]]
  | p `elem` [ PrimitiveIntNegate ]
                = do
    tyIntP  <- TyConstant <$> getPrimitive TypeConstructorName "Int_prim"
    tyInt  <- TyConstant <$> getBase TypeConstructorName "Int"
    return $ makeTypeApplication TyArrow [tyIntP, tyInt]
  | p `elem` [ PrimitiveIntLT, PrimitiveIntLE, PrimitiveIntGT, PrimitiveIntGE, PrimitiveIntEQ, PrimitiveIntNE ]
                = do
    tyIntP  <- TyConstant <$> getPrimitive TypeConstructorName "Int_prim"
    tyBool <- TyConstant <$> getPrimitive TypeConstructorName "Bool"
    return $ makeTypeApplication TyArrow [tyIntP, makeTypeApplication TyArrow [tyIntP, tyBool]]
  | p `elem` [ PrimitiveOrd ]
                = do
    tyInt  <- TyConstant <$> getBase TypeConstructorName "Int"
    tyCharP <- TyConstant <$> getPrimitive TypeConstructorName "Char_prim"
    return $ makeTypeApplication TyArrow [tyCharP, tyInt]
  | p `elem` [ PrimitiveChr ]
                = do
    tyIntP  <- TyConstant <$> getPrimitive TypeConstructorName "Int_prim"
    tyChar <- TyConstant <$> getBase TypeConstructorName "Char"
    return $ makeTypeApplication TyArrow [tyIntP, tyChar]
  | p `elem` [ PrimitiveCharLT, PrimitiveCharLE, PrimitiveCharGT, PrimitiveCharGE, PrimitiveCharEQ, PrimitiveCharNE ]
                = do
    tyCharP <- TyConstant <$> getPrimitive TypeConstructorName "Char_prim"
    tyBool <- TyConstant <$> getPrimitive TypeConstructorName "Bool"
    return $ makeTypeApplication TyArrow [tyCharP, makeTypeApplication TyArrow [tyCharP, tyBool]]
  | p `elem` [ PrimitiveBindIO ]
                = do
    tyIO   <- makeTypeApplication . TyConstant <$> getPrimitive TypeConstructorName "IO"
    a      <- TyVariable <$> generateName
    b      <- TyVariable <$> generateName
    let tyArrow = makeTypeApplication TyArrow
    return $ tyArrow [tyIO [a], tyArrow [tyArrow [a, tyIO [b]], tyIO [b]]]
  | p `elem` [ PrimitiveReturnIO ]
                = do
    tyIO   <- makeTypeApplication . TyConstant <$> getPrimitive TypeConstructorName "IO"
    a      <- TyVariable <$> generateName
    return $ makeTypeApplication TyArrow [a, tyIO [a]]
  | p `elem` [ PrimitivePutChar ]
                = do
    tyIO   <- makeTypeApplication . TyConstant <$> getPrimitive TypeConstructorName "IO"
    tyCharP <- TyConstant <$> getPrimitive TypeConstructorName "Char_prim"
    tyUnit <- TyConstant <$> getPrimitive TypeConstructorName "()"
    return $ makeTypeApplication TyArrow [tyCharP, tyIO [tyUnit]]
  | p `elem` [ PrimitiveError ]
                = do
    tyList <- makeTypeApplication . TyConstant <$> getPrimitive TypeConstructorName "[]"
    tyChar <- TyConstant <$> getBase TypeConstructorName "Char"
    a      <- TyVariable <$> generateName
    return $ makeTypeApplication TyArrow [tyList [tyChar], a]

typecheckBindings :: ModuleName -> DeclarationMap CoreName -> TypecheckMonad C.TypeClassDeclarationMap
typecheckBindings md bs = do
    let bgs = reverse $ topologicalSort (Set.fromList $ Map.keys mBindings) []
    Map.mapKeys (\(QName _ VariableName k) -> k) <$> foldlM
      (\bts bg -> (Map.union bts <$>) $ globalBindMany (Map.map fst bts) $ (Map.union bts) <$> typecheckBindings' bg)
      Map.empty
      bgs
  where
    mBindings = Map.mapKeys (QName md VariableName) bs

    dependenciesMapStep :: Map QCoreName (Set QCoreName) -> Map QCoreName (Set QCoreName)
    dependenciesMapStep mp = Map.mapWithKey (\k v -> Set.union v $ Set.unions $ fromJust . flip Map.lookup mp <$> Set.toList v) mp

    makeDependenciesMap :: Int -> Map QCoreName (Set QCoreName) -> Map QCoreName (Set QCoreName)
    makeDependenciesMap 0 = id
    makeDependenciesMap n = makeDependenciesMap (n `div` 2) . dependenciesMapStep

    dependenciesMap :: Map QCoreName (Set QCoreName)
    dependenciesMap = makeDependenciesMap (Map.size mBindings) (Map.map (Set.intersection (Map.keysSet mBindings) . declarationFreeVariables) mBindings)

    reverseDependenciesMap :: Map QCoreName (Set QCoreName)
    reverseDependenciesMap = Map.foldWithKey (\k -> flip . Set.fold . Map.update $ Just . Set.insert k) (Map.map (const Set.empty) mBindings) dependenciesMap

    topologicalSort :: Set QCoreName -> [Set QCoreName] -> [Set QCoreName]
    topologicalSort st acc
      | Set.null st = acc
      | otherwise   =
          let elem       = Set.findMin st
              allElemDep = fromJust $ Map.lookup elem dependenciesMap
              revElemDep = fromJust $ Map.lookup elem reverseDependenciesMap
              elemSet    = Set.insert elem $ Set.intersection allElemDep revElemDep
              elemDep    = allElemDep `Set.difference` elemSet
              acc'       = topologicalSort elemDep acc
            in
          topologicalSort (Set.delete elem $ st `Set.difference` allElemDep) (elemSet : acc')

    typecheckBindings' :: Set QCoreName -> TypecheckMonad (Map QCoreName (PolyType CoreName, C.TypeClassDeclaration))
    typecheckBindings' st = do
      let xs = Set.toList st
          es = fromJust . flip Map.lookup mBindings <$> xs
      ts <- forM xs (const $ TyVariable <$> generateName)
      esTy <- globalBindMany (Map.fromList $ zip xs (PolyType Map.empty <$> ts)) $ forM (zip es ts) $ \(e, t) -> do
        eTy <- typecheckDeclaration e
        case eTy of
          C.Declaration e             -> liftUnify $ unifyType t (C.typeclassExpressionType e)
          C.PrimitiveDeclaration prim -> do
            pTy <- primitiveType prim
            liftUnify $ unifyType t pTy
        return $ eTy
      ts    <- liftUnify $ substituteType `mapM` ts
      freeG <- environmentVariables
      tcs <- forM ts $ \t -> do
        let fvs = (`Set.difference` freeG) $ freeTypeVariables t
        fmap Map.fromList $ forM (Set.toList fvs) $ \v -> do
          cs <- liftUnify $ getConstraints v
          return (v, cs)
      let tvs = uncurry PolyType <$> zip tcs ts
      return $ Map.fromList $ zip xs (zip tvs esTy)

typecheckDataConstructor :: ModuleName -> MonoType CoreName -> Set CoreName -> DataConstructor CoreName -> TypecheckMonad (C.DataConstructor, Map QCoreName (PolyType CoreName))
typecheckDataConstructor md ty tvs (DataConstructor n ts) = do
  let cname = QName md ConstructorName n
      ctype = PolyType (Map.fromSet (const Set.empty) tvs) $ foldr (\a b -> makeTypeApplication TyArrow [a, b]) ty ts
  return $ (C.DataConstructor cname ts ctype, Map.singleton cname ctype)

typecheckTypeDeclaration :: ModuleName -> QCoreName -> TypeDeclaration CoreName -> TypecheckMonad (Maybe C.DataDeclaration, Map QCoreName (PolyType CoreName))
typecheckTypeDeclaration md n (DataDeclaration tvs dcs) = do
  let ty  = makeTypeApplication (TyConstant n) (TyVariable <$> tvs)
  dcs' <- typecheckDataConstructor md ty (Set.fromList tvs) `mapM` dcs
  return (Just $ C.DataDeclaration tvs (fst <$> dcs'), Map.unions (snd <$> dcs'))
typecheckTypeDeclaration md n (PrimitiveDataDeclaration prim) = return (Just $ C.PrimitiveDataDeclaration prim, Map.empty)

typecheckTypeDeclarations :: ModuleName -> TypeDeclarationMap CoreName -> TypecheckMonad (C.DataDeclarationMap, Map QCoreName (PolyType CoreName))
typecheckTypeDeclarations md ds = do
  ds' <- forM (Map.toList ds) $ \(k, v) -> do
    let name = QName md TypeConstructorName k
    (a, b) <- typecheckTypeDeclaration md name v
    return (maybe Nothing (\a -> Just (name, a)) a, b)
  return (Map.fromList . fmap fromJust . filter isJust $ fst <$> ds', Map.unions $ snd <$> ds')

typecheckClassDeclaration :: ModuleName -> CoreName -> ClassDeclaration CoreName -> TypecheckMonad ( (QCoreName, C.DataDeclaration)
                                                                                               , C.TypeClassDeclarationMap
                                                                                               , Map QCoreName (PolyType CoreName)
                                                                                               )
typecheckClassDeclaration md cls (ClassDeclaration v ms) = do
  let cls_type = TyApplication (TyConstant $ QName md TypeClassName cls) (TyVariable v)
  (fvs, ds) <- fmap (Set.delete v . Set.unions *** Map.fromList) . fmap unzip $ forM (zip [0..] $ Map.toList ms) $ \(i, (n, ty)) -> do
    let fvs = freeTypeVariables ty
    when (not (Set.member v fvs)) $ error $ "class ty var " ++ show v ++ " should appear in class method signature " ++ show fvs
    cls_x  <- generateName
    cls_vs <- forM (Map.toList ms) $ const generateName
    base_id <- getBase VariableName "id"
    return ( fvs
           , ( n
             , ( PolyType (Map.insert v (Set.singleton $ QName md TypeClassName cls) $ Map.fromSet (const Set.empty) fvs) ty
               , C.Declaration
                 $ C.Expression (ty, []) $ C.ECase (C.Expression (cls_type, [(QName md TypeClassName cls, TyVariable v)])
                                                    $ C.EVariable base_id
                                                   )
                 $ flip C.PData Nothing
                 $ Map.singleton (QName md ConstructorName cls)
                 $ ( cls_vs
                   , C.Expression (ty, []) $ C.EVariable $ QName [] VariableName $ cls_vs !! i
                   )
               )
             )
           )
  let ctype = PolyType (Map.insert v Set.empty $ Map.fromSet (const Set.empty) fvs)
              $ foldr
              (\a b -> makeTypeApplication TyArrow [a, b])
              cls_type
              (fmap snd $ Map.toList ms)
      dd = C.DataDeclaration [v] [C.DataConstructor (QName md ConstructorName cls) (fmap snd $ Map.toList ms) ctype]
  return ((QName md TypeClassName cls, dd), ds, Map.singleton (QName md ConstructorName cls) ctype)

typecheckClassDeclarations :: ModuleName -> ClassDeclarationMap CoreName -> TypecheckMonad (C.DataDeclarationMap, C.TypeClassDeclarationMap, Map QCoreName (PolyType CoreName))
typecheckClassDeclarations md cs = do
  fmap (\(a, b, c) -> (Map.fromList a, Map.unions b, Map.unions c)) . fmap unzip3 $ (uncurry $ typecheckClassDeclaration md) `mapM` Map.toList cs

addUnifyInstances :: ModuleName -> InstanceDeclarationMap CoreName -> TypecheckMonad ()
addUnifyInstances md is = do
  forM_ (Map.toList is) $ \((cls, ty), _) -> do
    liftUnify $ addInstance cls ty

typecheckInstances :: ModuleName -> InstanceDeclarationMap CoreName -> TypecheckMonad (Map (QCoreName, QCoreName) C.TypeClassExpression)
typecheckInstances md is = do
  fmap Map.fromList $ forM (Map.toList is) $ \((cls, ty), InstanceDeclaration tvs decls) -> do
    let (TyConstant a, _) = typeApplicationDecompose ty []
        QName cls_md _ cls_n = cls
    e <- typecheckExpression
      $ Locate noLocation $ ELet decls
      $ makeApplication (Locate noLocation $ EVariable (QName cls_md ConstructorName cls_n))
      $ (Locate noLocation . EVariable . QName [] VariableName <$> Map.keys decls)
    return ((cls, a), e)
    

resolvePatternGroupInstances :: C.TypeClassPatternGroup -> TypecheckMonad C.PatternGroup
resolvePatternGroupInstances (C.PData cs df) = do
  cs' <- mapM (\(vs, e) -> liftM ((,) vs) (resolveExpressionInstances e)) cs
  df' <- mapM resolveExpressionInstances df
  return $ C.PData cs' df'

resolveExpressionInstances' :: C.TypeClassExpression' -> TypecheckMonad C.Expression'
resolveExpressionInstances' (C.EInteger i)       = return $ C.EInteger i
resolveExpressionInstances' (C.EChar c)          = return $ C.EChar c
resolveExpressionInstances' (C.EVariable v)      = return $ C.EVariable v
resolveExpressionInstances' (C.EApplication f t) = C.EApplication <$> resolveExpressionInstances f <*> resolveExpressionInstances t
resolveExpressionInstances' (C.ELambda x e)      = C.ELambda x <$> resolveExpressionInstances e
resolveExpressionInstances' (C.ELet bs e)        = C.ELet <$> resolveBindingsInstances bs <*> resolveExpressionInstances e
resolveExpressionInstances' (C.ECase e p)        = C.ECase <$> resolveExpressionInstances e <*> resolvePatternGroupInstances p

resolveExpressionInstances :: C.TypeClassExpression -> TypecheckMonad C.Expression
resolveExpressionInstances (C.Expression (ty, cs) e) = (if null cs then id else traceShow (ty, cs)) $ do
  e' <- resolveExpressionInstances' e
  cs' <- forM cs $ \(cls, ty) -> do
    ty' <- liftUnify $ substituteType ty
    traceShow (ty, "->", ty') $ return ()
    buildInstance cls ty'
  return $ C.makeApplication (C.Expression ty e') cs'
  where
    buildInstance :: QCoreName -> MonoType CoreName -> TypecheckMonad C.Expression
    buildInstance cls (TyVariable v) = do
      n <- Map.lookup (cls, v) . environmentInstances <$> ask
      case n of
       Nothing -> do
         aaa <- liftUnify $ getConstraints v
         error $ "Instance error : " ++ show cls ++ " " ++ show v
       Just e  -> traceShow (v, e) $ return e
    buildInstance cls t = traceShow (cls, t) $ do
      let (TyConstant a, bs) = typeApplicationDecompose t []
      c <- Map.lookup (cls, a) . environmentInstanceConstructors <$> ask
      case c of
       Nothing -> error $ "Instance constructor error : " ++ show cls ++ " " ++ show a
       Just c  -> resolveExpressionInstances c

resolvePolyExpressionInstances :: PolyType CoreName -> C.TypeClassExpression -> TypecheckMonad (PolyType CoreName, C.Expression)
resolvePolyExpressionInstances ty@(PolyType cs _) e = do
  (ins, is) <- fmap (unzip . concat) $ forM (Map.toList cs) $ \(tv, c) ->
    forM (Set.toList c) $ \cls -> do
      n <- generateName
      traceShow (cls, n, tv) $ return ()
      return ((cls, n), ((cls, tv), C.Expression (TyConstant cls) (C.EVariable (QName [] VariableName n))))
  e' <- bindInstances (Map.fromList is) $ resolveExpressionInstances e
  let e'' = foldr
            (\(cls, n) e ->
              C.Expression
              (makeTypeApplication TyArrow [TyConstant cls, C.expressionType e])
              (C.ELambda n e)
            )
            e'
            ins
  return $ ( PolyType
             (fmap (const Set.empty) cs)
             (C.expressionType e'') 
           , e''
           )

resolveBindingsInstances :: C.TypeClassBindingMap -> TypecheckMonad C.BindingMap
resolveBindingsInstances mp = traverse (uncurry resolvePolyExpressionInstances) mp

resolveDeclarationInstances :: (PolyType CoreName, C.TypeClassDeclaration) -> TypecheckMonad (PolyType CoreName, C.Declaration)
resolveDeclarationInstances (ty, C.Declaration e)          = do
  (ty', e') <- resolvePolyExpressionInstances ty e
  return (ty', C.Declaration e')
  
resolveDeclarationInstances (ty, C.PrimitiveDeclaration p) = return $ (ty, C.PrimitiveDeclaration p)

resolveDeclarationsInstances :: C.TypeClassDeclarationMap -> TypecheckMonad C.DeclarationMap
resolveDeclarationsInstances mp = do
  fmap Map.fromDistinctAscList $ forM (Map.toList mp) $ \(k, d) -> do
    d' <- resolveDeclarationInstances d
    return (k, d')

typecheckModule :: Module CoreName -> TypecheckMonad ( Map QCoreName (PolyType CoreName)
                                                   , Map (QCoreName, QCoreName) C.TypeClassExpression
                                                   , C.Module
                                                   )
typecheckModule (Module md is ds cs ids bs) = do
  (dds', ds')      <- typecheckTypeDeclarations md ds
  (cds', cs', ccs) <- typecheckClassDeclarations md cs
  addUnifyInstances md ids
  (ids', decls)    <- globalBindMany (Map.map fst . Map.mapKeys (QName md VariableName) $ cs')
                      $ globalBindMany ccs
                      $ globalBindMany ds'
                      $ do
                        bs'   <- typecheckBindings md bs
                        ids'  <- globalBindMany (Map.map fst . Map.mapKeys (QName md VariableName) $ bs')
                                 $ typecheckInstances md ids
                        decls <- fmap (uncurry Map.union) $ bindInstanceConstructors ids'
                                 $ liftM2 (,)
                                 (resolveDeclarationsInstances cs')
                                 (resolveDeclarationsInstances bs')
                        return (ids', decls)
  return ( Map.unions [ ds'
                      , ccs
                      , Map.map fst . Map.mapKeys (QName md VariableName) $ decls
                      ]
         , ids'
         , C.Module md (Map.union dds' cds') decls
         )
