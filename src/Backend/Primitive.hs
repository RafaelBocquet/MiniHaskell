module Backend.Primitive where

import Syntax.Name
import qualified Syntax.Expression as S

import Backend.Mips
import Backend.Mangle
import Backend.Runtime

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

import Prelude hiding (div, rem, seq)

codegenPrimitive :: S.PrimitiveDeclaration -> SectionMonad ()
codegenPrimitive p | p `elem` [S.PrimitiveIntAdd, S.PrimitiveIntSub, S.PrimitiveIntMul, S.PrimitiveIntDiv, S.PrimitiveIntRem] = do
  lw  t0 0 sp
  lw  t1 4 sp
  case p of
   S.PrimitiveIntAdd -> add t0 t0 t1
   S.PrimitiveIntSub -> sub t0 t0 t1
   S.PrimitiveIntMul -> mul t0 t0 t1
   S.PrimitiveIntDiv -> div t0 t0 t1
   S.PrimitiveIntRem -> rem t0 t0 t1
  add sp sp (8 :: Int)
  l a0 (12 :: Int)
  l v0 (9 :: Int)
  syscall
  l a0 =<< global "_runtime_continue"
  sw a0 0 v0
  l a0 (1 :: Int)
  sw a0 4 v0
  sw t0 8 v0
  l rt v0
  j =<< global "_runtime_continue"
codegenPrimitive S.PrimitiveIntNegate = do
  lw t0 0 sp
  sub t0 zero t0
  l a0 (12 :: Int)
  l v0 (9 :: Int)
  syscall
  l a0 =<< global "_runtime_continue"
  sw a0 0 v0
  l a0 (1 :: Int)
  sw a0 4 v0
  sw t0 8 v0
  l rt v0
  j =<< global "_runtime_continue"
codegenPrimitive p | p `elem` [S.PrimitiveIntEQ, S.PrimitiveIntNE, S.PrimitiveIntLT, S.PrimitiveIntLE, S.PrimitiveIntGT, S.PrimitiveIntGE] = do
  lw  t0 0 sp
  lw  t1 4 sp
  add sp sp (8 :: Int)
  case p of
   S.PrimitiveIntEQ -> sne  t0 t0 t1
   S.PrimitiveIntNE -> seq  t0 t0 t1
   S.PrimitiveIntLT -> sge  t0 t0 t1
   S.PrimitiveIntLE -> sgt  t0 t0 t1
   S.PrimitiveIntGT -> sle  t0 t0 t1
   S.PrimitiveIntGE -> slt  t0 t0 t1
  add t0 t0 (1 :: Int)
  -- Bool : 1 ~ True
  --        2 ~ False
  l  a0 (8 :: Int)
  l  v0 (9 :: Int)
  syscall
  l  a0 =<< global "_runtime_continue"
  sw a0 0 v0
  sw t0 4 v0
  l rt v0
  j =<< global "_runtime_continue"
codegenPrimitive S.PrimitiveBindIO   = do
  bindLabel <- newLabel
  flipLabel <- newLabel
  
  lw t0 0 sp
  lw t1 4 sp
  add sp sp (8 :: Int)
  l  a0 (20 :: Int)
  l  v0 (9 :: Int)
  syscall
  l  a0 =<< global "_runtime_continue"
  sw a0 0 v0
  l  a0 bindLabel
  sw a0 8 v0
  l  rt v0
  add v0 v0 (8 :: Int)
  sw v0 4 rt
  sw t0 4 v0
  sw t1 8 v0
  j =<< global "_runtime_continue"

  label bindLabel
  lw t0 8 rt
  sub sp sp (16 :: Int)
  sw t0 8 sp
  l  t0 flipLabel
  sw t0 4 sp
  l  t0 =<< global "_runtime_perform_io"
  sw t0 0 sp
  sw t0 12 sp
  lw rt 4 rt
  j =<< global "_runtime_apply_continuation_0"

  label flipLabel
  l  t0 rt
  lw rt 0 sp
  sw t0 0 sp
  j =<< global "_runtime_apply_continuation_1"
codegenPrimitive S.PrimitiveReturnIO = do
  returnLabel <- newLabel
  
  lw t0 0 sp
  add sp sp (4 :: Int)
  l  a0 (16 :: Int)
  l  v0 (9 :: Int)
  syscall
  l  a0 =<< global "_runtime_continue"
  sw a0 0 v0
  l  a0 returnLabel
  sw a0 8 v0
  l  rt v0
  add v0 v0 (8 :: Int)
  sw v0 4 rt
  sw t0 4 v0
  j =<< global "_runtime_continue"

  label returnLabel
  lw rt 4 rt
  j =<< global "_runtime_continue"
codegenPrimitive S.PrimitivePutChar  = do
  putCharLabel <- newLabel
  
  lw t0 0 sp
  add sp sp (4 :: Int)
  l  a0 (16 :: Int)
  l  v0 (9 :: Int)
  syscall
  l  a0 =<< global "_runtime_continue"
  sw a0 0 v0
  l  a0 putCharLabel
  sw a0 8 v0
  l  rt v0
  add v0 v0 (8 :: Int)
  sw v0 4 rt
  sw t0 4 v0
  j =<< global "_runtime_continue"

  label putCharLabel
  lw a0 4 rt
  l  v0 (11 :: Int)
  syscall
  l  rt =<< global "_data_unit"
  j =<< global "_runtime_continue"
  
