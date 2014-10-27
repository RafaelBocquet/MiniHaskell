module Backend.Codegen where

import Syntax.Name

import Core.Expression
import Core.Module

import Backend.Mips

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

data IRegister = Unknown Int
               | Known ERegister
data ERegister = Physical MipsRegister
               | Stack Int
               | Block Int

data Instruction r = IConst Int r
                   | ILoadGlobal QCoreName r
                   | IApply r r r
                   | ILet [(r, [r], Int, [EInstruction])]
                   | IDataCase r (Map Int ([r], [Instruction r])) (Maybe [Instruction r]) r
                   | IMove r r

type IInstruction = Instruction IRegister
type EInstruction = Instruction ERegister

explicitRegisters :: [IInstruction] -> [EInstruction]
explicitRegisters = undefined

data CodegenEnvironment = CodegenEnvironment
  { codegenConstructorTags   :: Map QCoreName Int
  , codegenVariableRegisters :: Map CoreName IRegister
  }

type CodegenMonad a = ReaderT CodegenEnvironment (WriterT (Endo [IInstruction]) (State Int)) a

runCodegen :: Map QCoreName Int -> CodegenMonad () -> State Int [EInstruction]
runCodegen tags m = fmap (explicitRegisters . flip appEndo [])
                    $ execWriterT
                    $ flip runReaderT (CodegenEnvironment tags Map.empty)
                    $ m

localCodegen :: CodegenMonad () -> CodegenMonad [EInstruction]
localCodegen m = explicitRegisters . flip appEndo [] . snd <$> local (\s -> s { codegenConstructorTags = codegenConstructorTags s
                                                                              , codegenVariableRegisters = Map.empty
                                                                              }) (listen m)

newRegister :: CodegenMonad IRegister
newRegister = do
  c <- get
  modify (+ 1)
  return $ Unknown c

constructorTag :: QCoreName -> CodegenMonad Int
constructorTag dc = fromJust . Map.lookup dc . codegenConstructorTags <$> ask

getVariable :: CoreName -> CodegenMonad IRegister
getVariable v = fromJust . Map.lookup v . codegenVariableRegisters <$> ask

bindVariable :: CoreName -> CodegenMonad a -> CodegenMonad (IRegister, a)
bindVariable v m = do
  r <- newRegister
  a <- local (\s -> s { codegenVariableRegisters = Map.insert v r (codegenVariableRegisters s) }) m
  return (r, a)

bindVariables :: [CoreName] -> CodegenMonad a -> CodegenMonad ([IRegister], a)
bindVariables vs m = do
  rs <- forM vs $ const newRegister
  a <- local (\s -> s { codegenVariableRegisters = Map.union (Map.fromList $ zip vs rs) (codegenVariableRegisters s) }) m
  return (rs, a)

constInt :: Int -> CodegenMonad IRegister
constInt i = do
  r <- newRegister
  tell (Endo (IConst i r :))
  return r

application :: IRegister -> IRegister -> CodegenMonad IRegister
application f t = do
  r <- newRegister
  tell (Endo (IApply f t r :))
  return r

loadGlobal :: QCoreName -> CodegenMonad IRegister
loadGlobal v = do
  r <- newRegister
  tell (Endo (ILoadGlobal v r :))
  return r

closure :: CoreName -> Expression -> CodegenMonad IRegister
closure x e = do
  let fvs = Set.toList $ Set.delete x $ expressionFreeVariables e
  r <- newRegister
  (fvrs, es) <- do
    fvrs <- getVariable `mapM` fvs
    -- TODO bind Variables, from Stack registers
    es <- localCodegen $ do
          r <- codegenExpression e
          tell (Endo (IMove r (Known (Physical rt)) :))
    return (fvrs, es)
  tell (Endo (ILet [(r, fvrs, length fvrs + 1, es)] :))
  return r
  
codegenModule :: Module -> MipsMonad ()
codegenModule (Module md dds ds) = undefined

codegenDeclaration :: Declaration -> CodegenMonad IRegister
codegenDeclaration (Declaration e) = codegenExpression e

codegenExpression :: Expression -> CodegenMonad IRegister
codegenExpression = codegenExpression' . expressionValue
  where
    codegenExpression' :: Expression' -> CodegenMonad IRegister
    codegenExpression' (EInteger i)                          = constInt i
    codegenExpression' (EChar c)                             = constInt (ord c)
    codegenExpression' (EVariable (QName [] VariableName v)) = getVariable v
    codegenExpression' (EVariable v)                         = loadGlobal v
    codegenExpression' (EApplication f t)                    = do
      rf              <- codegenExpression f
      rt              <- codegenExpression t
      application rf rt
    codegenExpression' (ELambda x e)                         = do
      closure x e
    codegenExpression' (ELet ds e)                           = do
      let vs = fst <$> Map.toList ds
      (rs, (fvrs, es)) <- bindVariables vs $ do
        ds <- forM (snd . snd <$> Map.toList ds) $ \d -> do
          let fvs = Set.toList $ declarationFreeVariables d
          fvrs <- getVariable `mapM` fvs
          es <- localCodegen $ do
            r <- codegenDeclaration d
            tell (Endo (IMove r (Known (Physical rt)) :))
          return (fvrs, es)
        return (fst <$> ds, snd <$> ds)
      tell (Endo (ILet (zip4 rs fvrs (length <$> fvrs) es) :))
      codegenExpression e
    codegenExpression' (ECase e (PData alts df))             = do
      or  <- newRegister
      re  <- codegenExpression e
      df' <- case df of
        Nothing -> return Nothing
        Just df -> do
          (r, is) <- listen $ codegenExpression df
          return $ Just $ appEndo is [IMove r or]
      alts' <- fmap Map.fromList $ forM (Map.toList alts) $ \(k, (vs, ec)) -> do
        tag           <- constructorTag k
        ((rs, r), is) <- listen $ bindVariables vs $ codegenExpression ec
        return (tag, (rs, appEndo is [IMove r or]))
      tell (Endo (IDataCase re alts' df' or :))
      return or
