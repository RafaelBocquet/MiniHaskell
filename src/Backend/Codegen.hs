module Backend.Codegen where

import Syntax.Name
import qualified Syntax.Expression as S

import Reg.Expression
import Reg.Graph

import Backend.Mips hiding (div)
import Backend.Mangle
import Backend.Runtime

import Backend.Primitive

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

-- Use v0, v1 as temporary registers

codegenExpression :: Expression -> SectionMonad ()
codegenExpression e = do
  let (stk, c) = colorGraph (makeGraph e) Map.empty 0
  sub sp sp stk
  codegenExpression' e c stk
  where
--    codegenExpression' e _ _ | traceShow e False = undefined
    codegenExpression' (EApplication (ALocal f) []) c sSize     = do
      case fromJust $ Map.lookup f c of
       Physical r -> do
         l rt r
       Stack s    -> do
         lw rt (4 * s) sp
      add sp sp (4 * sSize)
      j =<< global "_runtime_apply_continuation_0"
    -- TODO : we must resize the stack accordingly
    -- Application to known functions
    codegenExpression' (EApplication f xs) c sSize = do
      case f of
       AGlobal n _ -> l rt =<< global ("_closure_" ++ mangle n)
       ALocal v -> case fromJust $ Map.lookup v c of
         Physical r -> do
           l rt r
         Stack s    -> do
           lw rt (4 * s) sp
       AConst i ->     do
         makeConstant i
         l rt v0
      sub sp sp (4 * nStackSize)
      forM [0 .. sSize - 1] $ \i -> do
        lw v0 (4 * nStackSize + 4 * i) sp
        sw v0 (4 * i) sp
      forM (zip xs [0..]) $ uncurry pushArgument
      add sp sp (4 * sSize)
      forM [1..nxs `div` maxSingleApplication] $ \i -> do
        l v0 =<< global ("_runtime_apply_continuation_" ++ show (if i == nxs `div` maxSingleApplication then nxs `mod` maxSingleApplication else maxSingleApplication))
        sw v0 (4 * i * (maxSingleApplication + 1) - 4) sp
      j =<< global ("_runtime_apply_continuation_" ++ show (min maxSingleApplication nxs))
      where
        nxs              = length xs
        nStackSize       = nxs + nxs `div` maxSingleApplication
        stackSlot i      = sSize + i + i `div` maxSingleApplication
        pushArgument x i = case x of
          ALocal v -> case fromJust $ Map.lookup v c of
            Physical r ->
              sw r (4 * (stackSlot i)) sp
            Stack j    -> do
              lw v0 (4 * j) sp
              sw v0 (4 * (stackSlot i)) sp
          AGlobal n _ -> do
            n_label <- global $ "_closure_" ++ mangle n
            l v0 n_label
            sw v0 (4 * (stackSlot i)) sp
          AConst n    -> do
            makeConstant n
            sw v0 (4 * (stackSlot i)) sp
        makeConstant i = do
          l a0 (12 :: Int)
          l v0 (9 :: Int)
          syscall
          l a0 =<< global "_runtime_continue"
          sw a0 0 v0
          l a0 (1 :: Int) -- Tag for Int or Char
          sw a0 4 v0
          l a0 i
          sw a0 8 v0
          
    codegenExpression' (ELet bs e) c sSize = do
      c_labels <- fmap Map.fromList $ forM bs $ \(x, _, _) -> do
        l <- newLabel
        return (x, l)
      let nVars                        = Set.fromList $ fmap (\(x, _, _) -> x) bs
          (heapSize, heapOffsets, fvs) = foldr
                                         (\(x, vs, e) (i, os, acc) ->
                                           let fvs = Set.toList $ expressionFreeVariables e `Set.difference` Set.fromList (vs) in
                                           foldr
                                           (\v (i, os, acc) ->
                                             (i+4, os, Map.insertWith (++) v [i] acc)
                                           )
                                           (i+4, Map.insert x i os, acc)
                                           fvs
                                         )
                                         (0, Map.empty, Map.empty)
                                         bs
      -- Allocate memory on the heap
      l a0 (heapSize :: Int)
      l v0 (9 :: Int)
      syscall

      -- Copy free variables where needed
      forM_ (Map.toList fvs) $ \(v, os) -> do
        r <- if Set.member v nVars
          then do
               add v1 v0 (fromJust $ Map.lookup v heapOffsets)
               return v1
          else case fromJust $ Map.lookup v c of
             Physical r ->
               return r
             Stack i    -> do
               lw v1 (4 * i) sp
               return v1
        forM_ os $ \o -> do
          sw r o v0

      -- Add closure headers
      forM_ (Map.toList c_labels) $ \(x, lbl) -> do
        l  v1 lbl
        sw v1 (fromJust $ Map.lookup x heapOffsets) v0

      -- put the new variables in memory

      forM_ (Map.toList heapOffsets) $ \(x, o) -> do
        case fromJust $ Map.lookup x c of
         Physical r -> do
           l   r v0
           add r r o
         Stack s -> do
           l v1 v0
           add v1 v1 o
           sw v1 (4 * s) sp
        
      -- then proceed with the expression
      codegenExpression' e c sSize

      -- Add the closure code
      forM_ bs $ \(x, vs, e) -> do
        word (length vs) -- Arity info for apply functions. 0 = THUNK = PARTIAL ?
        label $ fromJust $ Map.lookup x c_labels
        -- rt is now a pointer to the closure
        -- arguments (vs are on the stack)
        -- => we have to move free variables
        let (stk, c) = colorGraph (makeGraph e) (Map.fromList $ zip vs (Stack <$> [-1,-2..])) 0
            c' = Map.map (\r -> case r of
                                 Stack i | i < 0 -> Stack (stk - i - 1)
                                 _               -> r
                         ) c
        -- Allocate memory on the stack
        sub sp sp stk
        -- Move the free variables
        -- The reverse is an ugly hack...
        let fvs = reverse $ Set.toList $ expressionFreeVariables e `Set.difference` Set.fromList vs
        forM (zip fvs [4,8..]) $ \(v, i) -> do
          case fromJust $ Map.lookup v c' of
            Physical r -> do
              lw r i rt
            Stack j    -> do
              lw v1 i rt
              sw v1 (4 * j) sp

        -- Eval expression
        codegenExpression' e c' (stk + length vs)
        
    codegenExpression' (EDataCase a alts df) c sSize = do
      let altsList     = Map.toList alts
      let fvs          = Set.toList $ Set.unions
                     $ maybe Set.empty expressionFreeVariables df
                     : fmap (\(_, (vs, e)) -> expressionFreeVariables e `Set.difference` Set.fromList vs) altsList
          (rfvs, sfvs) = partition (\v -> case fromJust $ Map.lookup v c of
                                    Physical _ -> True
                                    Stack _    -> False
                            ) fvs
          stackMod     = 1 + length rfvs
          c'           = Map.union
                         (Map.fromList $ zip rfvs (Stack <$> [0..]))
                         (Map.fromList $ fmap (\v -> let Stack i = fromJust $ Map.lookup v c in (v, Stack $ i + stackMod - 1)) sfvs)
      cont_label <- newLabel

      sub sp sp (4 * stackMod)
      -- Put free variables currently in physical registers on the stack
      forM_ (zip rfvs [4, 8..]) $ \(v, i) -> do
        let Physical r = fromJust $ Map.lookup v c
        sw r i sp
      -- Add continuation on the stack
      l v0 cont_label
      sw v0 0 sp
      -- Eval a
      
      case a of
        ALocal v -> case fromJust $ Map.lookup v c of
          Physical r -> do
            l rt r
          Stack i    -> do
            lw rt (4 * i + 4 * stackMod) sp
        AGlobal n _ ->
          l rt =<< global ("_closure_" ++ mangle n)
      j =<< global "_runtime_apply_continuation_0"

      -- Continuation
      -- a is now in WHNF, in rt
      label cont_label
      alt_labels <- forM altsList (const newLabel)
      -- tag in v0
      lw v0 4 rt
      -- Jump to correct case
      forM_ (zip altsList alt_labels) $ \((tag, (_, _)), lbl) -> do
        l v1 tag
        beq v0 v1 lbl
      -- Default case
      case df of
       Nothing -> do
         l v0 (-1 :: Int)
         syscall
       Just e  -> do
        let efvs = expressionFreeVariables e
            cstk = Map.foldr (\r acc -> case r of
                                         Physical _ -> acc
                                         Stack i    -> max acc (i + 1)
                             ) 0 c'
            (stk, c) = colorGraph (makeGraph e) c' cstk
        let c' = Map.map (\r -> case r of
                                 Stack i | i >= cstk -> Stack (i - cstk)
                                 Stack i            -> Stack (i + stk - cstk)
                                 Physical r         -> Physical r
                         ) c
        when (stk /= cstk) $ do
          sub sp sp (stk - cstk)
        codegenExpression' e c' (sSize + stackMod + stk - cstk - 1)
      -- Alt cases
      forM_ (zip altsList alt_labels) $ \((_, (vs, e)), lbl) -> do
        let efvs = expressionFreeVariables e
            cstk = Map.foldr (\r acc -> case r of
                                         Physical _ -> acc
                                         Stack i    -> max acc (i + 1)
                             ) 0 c'
            (stk, c) = colorGraph (addGraphEdges (makeGraph e) vs) c' cstk
        let c' = Map.map (\r -> case r of
                                 Stack i | i >= cstk -> Stack (i - cstk)
                                 Stack i            -> Stack (i + stk - cstk)
                                 Physical r         -> Physical r
                         ) c
        label lbl
        when (stk /= cstk) $ do
          sub sp sp (stk - cstk)
        forM_ (zip vs [8, 12..]) $ \(v, i) ->
          when (Set.member v efvs) $
          case fromJust $ Map.lookup v c' of
           Physical r -> do
             lw r i rt
           Stack j    -> do
             lw v0 i rt
             sw v0 (4 * j) sp
        codegenExpression' e c' (sSize + stackMod + stk - cstk - 1) -- -1 because the continuation was already pop'd from the stack by _runtime_continue !

codegenDataConstructor :: Int -> Int -> SectionMonad ()
codegenDataConstructor tag ar = do
  continue <- global "_runtime_continue"
  l a0 (4 * ar + 8)
  l v0 (9 :: Int)
  syscall
  
  l  v1 continue
  sw v1 0 v0

  l  v1 tag
  sw v1 4 v0

  forM [0..ar-1] $ \i -> do
    lw  v1 (4 * i) sp
    sw  v1 (8 + 4 * i) v0

  add sp sp (4 * ar)

  l rt v0
  j =<< global "_runtime_continue"


codegenDeclaration :: QName CoreName -> Declaration -> SectionMonad ()
codegenDeclaration name (Declaration e)                   = do
  word (0 :: Int)
  label =<< global (mangle name)
  codegenExpression e
codegenDeclaration name (PrimitiveDeclaration p)          = do
  word $ case p of
          _ | p `elem` [S.PrimitiveIntAdd, S.PrimitiveIntSub, S.PrimitiveIntMul, S.PrimitiveIntDiv, S.PrimitiveIntRem]              -> (2 :: Int)
          S.PrimitiveIntNegate                                                                                                      -> 1
          _ | p `elem` [S.PrimitiveIntEQ, S.PrimitiveIntNE, S.PrimitiveIntLT, S.PrimitiveIntLE, S.PrimitiveIntGT, S.PrimitiveIntGE] -> 2
          S.PrimitiveBindIO                                                                                                         -> 2
          S.PrimitiveReturnIO                                                                                                       -> 1
          S.PrimitivePutChar                                                                                                        -> 1
  label =<< global (mangle name)
  codegenPrimitive p
codegenDeclaration name (DataConstructorDeclaration t ar) = do
  word (ar :: Int)
  label =<< global (mangle name)
  codegenDataConstructor t ar
