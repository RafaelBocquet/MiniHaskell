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
               deriving (Eq, Ord, Show)
data ERegister = Physical MipsRegister
               | Stack Int
               deriving (Eq, Ord, Show)

data Instruction r = IConst Int r
                   | ILoadGlobal QCoreName r
                   | IApply r r r
                   | ILet [(r, [r], Int, [EInstruction])]
                   | IDataCase r (Map Int ([r], [Instruction r])) (Maybe [Instruction r])
                   | IMove r r

type IInstruction = Instruction IRegister
type EInstruction = Instruction ERegister

explicitRegisters :: [IInstruction] -> [EInstruction]
explicitRegisters = undefined

moveRegister :: ERegister -> ERegister -> SectionMonad ()
moveRegister r r' | r == r'                         = return ()
moveRegister (Physical r) (Physical r')            = do
  l  r' r
moveRegister (Physical r) (Stack r')               = do
  sw r  r' sp
moveRegister (Stack r) (Physical r')               = do
  lw r' r  sp
moveRegister (Stack r) (Stack r')                  = do
  lw t0 r  sp
  sw t0 r' sp

-- + Stack offset, because of continuations
codegenExplicitMips :: [EInstruction] -> SectionMonad ()
codegenExplicitMips is = do
  forM_ is codegenExplicitMips'
  where
    codegenExplicitMips' (IConst i (Physical r))                       = do
      li r  i
    codegenExplicitMips' (IConst i (Stack s))                          = do
      li t0 i
      sw t0 s sp
    codegenExplicitMips' (IMove r r')                                  = moveRegister r r'
    codegenExplicitMips' (IDataCase r alts df)              = do
      lbl_continuation <- newLabel
      alt_labels <- forM (Map.toList alts) (const newLabel)
      fail_label <- global "runtime_fail"
      
      sub sp sp (4 :: Int)
      l   t0 lbl_continuation
      sw  t0 0 sp  -- Setup continuation
      
      moveRegister r (Physical rt) -- Eval thunk
      lw  t0 0 rt
      j   t0

-- *  r, rt  *
--    |
--    v
-- * code_ptr * tag * data1 * data2 * ... *

      label lbl_continuation
      add sp sp (4 :: Int) -- Remove continuation from the top of the stack
      -- rt is now a pointer to an evaluated thunk
      lw t1 4 rt
      forM_ (zip (Map.toList alts) alt_labels) $ \((tag, _), lbl) -> do
        li  t0 tag
        beq t0 t1 lbl
      case df of
        Nothing -> j fail_label
        Just df -> codegenExplicitMips df
      forM_ (zip (Map.toList alts) alt_labels) $ \((_, (rs, is)), lbl) -> do
        label lbl
        forM_ (zip rs [8,16..]) $ \(r, i) -> do
          lw t1 i rt
          moveRegister (Physical t1) r
        codegenExplicitMips is
type ConstructorTags = Map QCoreName Int

data CodegenEnvironment = CodegenEnvironment
  { codegenConstructorTags   :: ConstructorTags
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

bindStackVariables :: [CoreName] -> CodegenMonad a -> CodegenMonad a
bindStackVariables vs m =
  local (\s -> s { codegenVariableRegisters = Map.union (Map.fromList $ zip vs (fmap (\i -> Known $ Stack i) [0..])) (codegenVariableRegisters s)}) m

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
    es <- bindStackVariables fvs $ localCodegen $ do
          r <- codegenExpression e
          tell (Endo (IMove r (Known (Physical rt)) :))
    return (fvrs, es)
  tell (Endo (ILet [(r, fvrs, length fvrs + 1, es)] :))
  return r

moduleConstructorTags :: Module -> ConstructorTags
moduleConstructorTags (Module _ dds _) = Map.unions $ fmap dataDeclarationTags $ Map.elems dds
  where
    dataDeclarationTags (DataDeclaration _ d)        = Map.fromList $ zip (dataConstructorName <$> d) [0..]
    dataDeclarationTags (PrimitiveDataDeclaration _) = Map.empty

codegenModule :: ConstructorTags -> Module -> MipsMonad ()
codegenModule tags (Module md dds ds) = do
  forM_ (Map.toList ds) $ \(name, decl) -> do
    -- DATA (in .data)
    dataSection $ do
      lbl_entry <- global "pouet_entry"
      label lbl_entry
      -- codegenExplicitMips decl
    -- CLOSURE (in .text)
    textSection $ do
      lbl_entry   <- global "pouet_entry"
      lbl_closure <- global "pouet_closure"
      label lbl_closure
      word lbl_entry
  
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
          es <- bindStackVariables fvs $ localCodegen $ do
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
      tell (Endo (IDataCase re alts' df' :))
      return or
