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

import Prelude hiding (div, rem)

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
