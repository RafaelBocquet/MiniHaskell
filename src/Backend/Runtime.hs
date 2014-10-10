module Backend.Runtime where

import Prelude hiding (error, putChar)

import Backend.Mips

putCharU :: MipsMonad ()
putCharU = do
  putCharU_Entry <- global "_putCharU_Entry"

  label putCharU_Entry

  l   a0 (97 :: Int)
  l   v0 (11 :: Int)
  syscall
  lw  t0 0 sp
  j   t0

putChar :: MipsMonad ()
putChar = do
  putCharU_Entry <- global "_putCharU_Entry"
  putChar_Entry <- global "_putChar_Entry"
  putChar_0 <- global "_putChar_0"

  label putChar_Entry

  sub sp sp (4 :: Int) -- Add continuation to _putChar_0 on top of the stack
  l   t0 putChar_0
  sw  t0 0 sp
  lw  rt 4 rt          -- Evaluate argument
  lw  t0 0 rt
  j   t0

  label putChar_0
  add sp sp (4 :: Int)
  j   putCharU_Entry

error :: MipsMonad ()
error = do
  error_Entry <- global "_error_Entry"
  error_0 <- global "_error_0"
  error_1 <- global "_error_1"
  putChar_Entry <- global "_putChar_Entry"

  label error_Entry

  sub sp sp (4 :: Int) -- Add continuation to error_0 on top of the stack
  l   t0 error_0
  sw  t0 0 sp
  lw  rt 4 rt          -- Evaluate argument
  lw  t0 0 rt
  j   t0

  label error_0        -- Call putChar#, then exit
  l   t0 error_1       -- Add continuation to error_1 on top of the stack
  sw  t0 0 sp
  sub hp hp (8 :: Int)
  l   t0 putChar_Entry
  sw  t0 0 hp
  sw  rt 4 hp
  l   rt hp
  j   putChar_Entry

  label error_1        -- Exit
  add sp sp (4 :: Int)
  l   v0 (10 :: Int)
  syscall
  
test_Main :: MipsMonad ()
test_Main = do
  l   a0 (1024 :: Int)
  l   v0 (9 :: Int)
  syscall
  l   hp v0
  add hp hp (1024 :: Int)

  error_Entry <- global "_error_Entry"
  callError <- global "callError"

  l   rt callError
  j   error_Entry

nop :: MipsMonad ()
nop = do
  nop_Closure <- global "nop_Closure"

  label nop_Closure
  lw  t0 0 sp
  j   t0

aChar :: MipsMonad ()
aChar = do
  aChar_Data <- global "aChar_Data"
  nop_Closure <- global "nop_Closure"

  label aChar_Data
  word nop_Closure
  word 'a'

callError :: MipsMonad ()
callError = do
  callError <- global "callError"
  error_Entry <- global "_error_Entry"
  aChar_Data <- global "aChar_Data"

  label callError
  word error_Entry
  word aChar_Data

textRuntime = do
  test_Main
  putCharU
  putChar
  error
  nop
dataRuntime = do
  aChar
  callError
