module Reg.Expression where

import Syntax.Name
import Syntax.Type
import qualified Syntax.Expression as S

import qualified Core.Expression as C
import qualified Core.Module as C

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Maybe
import Data.Char
import Data.List

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

import Debug.Trace

data Expression = EApplication Atom [Atom]
                | ELet [(CoreName, [CoreName], Expression)] Expression
                | EDataCase Atom (Map Int ([CoreName], Expression)) (Maybe Expression)
                | EIntCase Atom (Map Int Expression) (Maybe Expression)
                deriving (Show)

-- EAtom : ILoad

-- EApplication : case on first atom
-- [Atom] : registers or stack
-- if Atom is global : jump
-- else, indirect

-- ELet : make closures with free Variables

-- DataCase : make continuation, eval, then match

-- needed Registers :
-- EAtom : One out, maybe one in
-- EApplication : free vars in, rt out
-- ELet : free vars in, expr fv out
-- EDataCase : e + fv in, fv out

-- Make (Map CoreName Register), with Register = Physical Mips | Stack Int
-- Interference graph
-- Registers allocated in ELets, and in ECase bindings
-- ECases have to save all FVs of the sub expressions, since forcing the thunk can do anything to registers

data Atom = ALocal CoreName
          | AGlobal QCoreName Int
          | AConst Int
          deriving (Show)

data Declaration = Declaration Expression
                 | PrimitiveDeclaration S.PrimitiveDeclaration
                 | DataConstructorDeclaration Int Int
                 deriving (Show)

atomFreeVariables :: Atom -> Set CoreName
atomFreeVariables (ALocal v) = Set.singleton v
atomFreeVariables _          = Set.empty

expressionFreeVariables :: Expression -> Set CoreName
expressionFreeVariables (EApplication a as)   = Set.unions (atomFreeVariables a
                                                            : fmap atomFreeVariables as
                                                           )
expressionFreeVariables (ELet bs e)           = Set.unions (expressionFreeVariables e `Set.difference` (Set.fromList $ fmap (\(v, _, _) -> v) bs)
                                                            : fmap (\(v, vs, e) -> expressionFreeVariables e `Set.difference` Set.fromList (v : vs)) bs
                                                           )
expressionFreeVariables (EDataCase a alts df) = Set.unions (atomFreeVariables a
                                                            : maybe Set.empty expressionFreeVariables df
                                                            : fmap (\(_, (vs, e)) -> expressionFreeVariables e `Set.difference` (Set.fromList vs)) (Map.toList alts)
                                                           )

expressionVariables :: Expression -> Set CoreName
expressionVariables (EApplication a as)     = Set.unions (atomFreeVariables a
                                                          : fmap atomFreeVariables as
                                                         )
expressionVariables (ELet bs e)             = Set.unions ( expressionVariables e
                                                          : fmap (\(v, vs, e) -> Set.insert v $ expressionFreeVariables e `Set.difference` Set.fromList vs) bs
                                                         )
expressionVariables e@(EDataCase a alts df) = Set.unions (atomFreeVariables a
                                                          : maybe Set.empty expressionFreeVariables df
                                                          : fmap (\(_, (vs, e)) -> expressionFreeVariables e `Set.union` Set.fromList vs) (Map.toList alts)
                                                         )

-- Constructor Tags

type ConstructorTags = Map QCoreName Int

moduleConstructorTags :: C.Module -> ConstructorTags
moduleConstructorTags (C.Module _ dds _) = Map.unions (dataDeclarationConstructorTags <$> Map.elems dds)

dataDeclarationConstructorTags :: C.DataDeclaration -> ConstructorTags
dataDeclarationConstructorTags (C.DataDeclaration _ dcs)      = Map.fromList $ zip (C.dataConstructorName <$> dcs) [1..]
dataDeclarationConstructorTags (C.PrimitiveDataDeclaration _) = Map.empty

-- Global arities

type GlobalArities = Map QCoreName Int

moduleGlobalArities :: C.Module -> GlobalArities
moduleGlobalArities (C.Module md dds ds) =
  Map.union
  (Map.unions (dataDeclarationGlobalArities <$> Map.elems dds))
  (Map.fromList $ fmap (\(n, (PolyType _ ty, _)) -> (QName md VariableName n, typeArity ty)) $ Map.toList ds)


dataDeclarationGlobalArities :: C.DataDeclaration -> GlobalArities
dataDeclarationGlobalArities (C.DataDeclaration _ dcs)      = Map.fromList $ fmap (\dc -> (C.dataConstructorName dc, length $ C.dataConstructorArguments dc)) dcs
dataDeclarationGlobalArities (C.PrimitiveDataDeclaration _) = Map.empty

typeArity :: MonoType CoreName -> Int
typeArity (TyApplication (TyApplication TyArrow _) b) = 1 + typeArity b
typeArity _                                           = 0

-- RegMonad

data RegEnvironment = RegEnvironment
                      { regConstructorTags :: ConstructorTags
                      , regGlobalArities   :: GlobalArities  
                      }

type RegMonad a = ReaderT RegEnvironment(State Int) a

runReg :: RegEnvironment -> RegMonad a -> State Int a
runReg = flip runReaderT

generateName :: RegMonad CoreName
generateName = do
  i <- get
  modify (+ 1)
  return $ CoreName i "a"

constructorTag :: QCoreName -> RegMonad Int
constructorTag n = fromJust . Map.lookup n . regConstructorTags <$> ask

globalArity :: QCoreName -> RegMonad Int
globalArity n = fromJust . Map.lookup n . regGlobalArities <$> ask

regModules :: [C.Module] -> State Int (Map QCoreName Declaration)
regModules mods =
  let tags = Map.unions (moduleConstructorTags <$> mods) in
  let arities = Map.unions (moduleGlobalArities <$> mods) in
  runReg (RegEnvironment tags arities) $ fmap Map.unions $ regModule `mapM` mods

regModule :: C.Module -> RegMonad (Map QCoreName Declaration)
regModule (C.Module md dds ds) = do
  ds' <- fmap Map.fromList $ forM (Map.toList ds) $ \(n, (_, e)) -> case e of
    C.Declaration e             -> do
      e' <- simplReg Map.empty <$> regExpression e
      return (QName md VariableName n, Declaration e')
    C.PrimitiveDeclaration prim -> return (QName md VariableName n, PrimitiveDeclaration prim)
  tags    <- regConstructorTags <$> ask
  arities <- regGlobalArities   <$> ask
  dds' <- fmap Map.fromList $ forM (Map.toList tags) $ \(n, tag) ->
    return (n, DataConstructorDeclaration tag (fromJust $ Map.lookup n arities))
  return $ Map.union dds' ds'


-- This disallows 'Let's on unboxed values, and avoid some other copying
simplAtom :: Map CoreName Atom -> Atom -> Atom
simplAtom subst (ALocal n) = case Map.lookup n subst of
  Just x  -> simplAtom subst x
  Nothing -> ALocal n
simplAtom subst x          = x

simplReg :: Map CoreName Atom -> Expression -> Expression
simplReg subst (ELet bs e)           =
  let (bas, bs') = partition (\(_, vs, b) -> case (vs, b) of
                                             ([], EApplication _ []) -> True
                                             _                       -> False
                             ) bs
      subst' = Map.union subst $ Map.fromList (fmap (\(x, [], EApplication a []) -> (x, a)) bas)
  in
  (case bs' of
    [] -> id
    _  -> ELet (fmap (\(x, vs, b) -> (x, vs, simplReg subst' b)) bs')
  ) $ simplReg subst' e
simplReg subst (EApplication a as)   = EApplication (simplAtom subst a) (simplAtom subst <$> as)
simplReg subst (EDataCase a alts df) = EDataCase (simplAtom subst a) (Map.map (\(a, b) -> (a, simplReg subst b)) alts) (maybe Nothing (Just . simplReg subst) df)

regExpression :: C.Expression -> RegMonad Expression
regExpression e = regExpression' $ C.expressionValue e
  where
    regExpression' (C.EInteger i)                          = return $ EApplication (AConst i) []
    regExpression' (C.EChar c)                             = return $ EApplication (AConst (ord c)) []
    regExpression' (C.EVariable (QName [] VariableName v)) = return $ EApplication (ALocal v) []
    regExpression' (C.EVariable gname)                     = do
      arity <- globalArity gname
      return $ EApplication (AGlobal gname arity) []
    regExpression' (C.ELambda x e)                         = do
      e' <- regExpression e
      case e' of
        ELet [(f, xs, e)] (EApplication (ALocal f') []) | f == f' -> return $ ELet [(f, x : xs, e)] (EApplication (ALocal f) [])
        e'                                                       -> do
          f <- generateName
          return $ ELet [(f, [x], e')] (EApplication (ALocal f) [])
    regExpression' (C.EApplication f t)                    = do
      f' <- regExpression f
      t' <- regExpression t
      case (f', t') of
       (EApplication af xs, EApplication at []) -> return $ EApplication af (xs ++ [at])
       (EApplication af xs, _)                  -> do
         at <- generateName
         return $ ELet [(at, [], t')] (EApplication af (xs ++ [ALocal at]))
       (ELet bs (EApplication af xs), EApplication at []) -> return $ ELet bs (EApplication af (xs ++ [at]))
       (ELet bs (EApplication af xs), _)                  -> do
         at <- generateName
         return $ ELet ((at, [], t') : bs) (EApplication af (xs ++ [ALocal at]))
       (_, EApplication at [])                  -> do
         af <- generateName
         return $ ELet [(af, [], f')] (EApplication (ALocal af) [at])
       (_, _)                                   -> do
         af <- generateName
         at <- generateName
         return $ ELet [(af, [], f'), (at, [], t')] (EApplication (ALocal af) [ALocal at])
    regExpression' (C.ELet bs e)                           = do
      vs <- fmap (fmap fromJust . filter isJust) $ forM (Map.toList bs) $ \(n, (_, v)) -> do
        v' <- regExpression v
        return $ Just (n, [], v')
      e' <- regExpression e
      return $ ELet vs e'
    regExpression' (C.ECase e (C.PData alts df))           = do
      e' <- regExpression e
      alts' <- fmap Map.fromList $ forM (Map.toList alts) $ \(name, (vs, ec)) -> do
        ec' <- regExpression ec
        tag <- constructorTag name
        return (tag, (vs, ec'))
      df' <- maybe (return Nothing) (fmap Just . regExpression) df
      case e' of
        EApplication e' [] -> return $ EDataCase e' alts' df'
        _ -> do
          et <- generateName
          return $ ELet [(et, [], e')] (EDataCase (ALocal et) alts' df')
