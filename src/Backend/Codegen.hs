module Backend.Codegen where

import Syntax.Name

import Reg.Expression
import Reg.Graph

import Backend.Mips
import Backend.Mangle

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

-- Use v0, v1 as temporary registers

codegenExpression :: Expression -> SectionMonad ()
codegenExpression e =
  let (stk, c) = colorGraph (makeGraph e) 0 in
   codegenExpression' e c
  where
    -- Application to known functions
    codegenExpression' (EApplication (AGlobal f ar) xs) c = do
      f_label <- global $ mangle f
      case compare ar (length xs) of
       LT -> undefined
       EQ -> do -- Exact arity
         sub sp sp (4 * ar)
         forM (zip xs [4,8..]) $ uncurry pushArgument
       GT -> do
         sub sp sp (4 * (length xs + 1))
         forM (take ar $ zip xs [4,8..]) $ uncurry pushArgument
         -- TODO : push generic apply
         forM (drop ar $ zip xs [8,12..]) $ uncurry pushArgument
      j f_label
      where
        pushArgument x i = case x of
          ALocal v    -> case fromJust $ Map.lookup v c of
            Physical r ->
              sw r i sp
            Stack j    -> do
              lw v0 (4 * ar + j) sp
              sw v0 i sp
          AGlobal n _ -> do
            n_label <- global $ mangle n
            l v0 n_label
            sw v0 i sp
          AConst i    -> do
            l v0 i
            sw v0 i sp
    codegenExpression' (ELet bs e) c                      = do
      c_labels <- fmap Map.fromList $ forM bs $ \(x, _, _) -> do
        l <- newLabel
        return (x, l)
      let nVars                        = Set.fromList $ fmap (\(x, _, _) -> x) bs
          (heapSize, heapOffsets, fvs) = foldr
                                         (\(x, vs, e) (i, os, acc) ->
                                           let fvs = Set.toList $ expressionFreeVariables e `Set.difference` Set.fromList vs in
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
               lw v1 i sp
               return v1
        forM_ os $ \o -> do
          sw r o v0

      -- Add closure headers
      forM_ c_labels $ \(x, lbl) -> do
        l  v1 lbl
        sw v1 (fromJust $ Map.lookup x heapOffsets) v0
        
      -- then proceed with the expression
      codegenExpression' e

      -- Add the closure code
      forM_ (zip bs c_labels) $ \((x, vs, e), lbl) -> do
        label lbl
        -- rt is now a pointer to the closure
        -- arguments (vs are on the stack)
        -- => we have to move free variables
        let (stk, c') = colorGraph (makeGraph e) 0
        -- Allocate memory on the stack
        
        codegenExpression' e
        
