module Backend.Runtime where

import Prelude hiding (error, putChar)

import Control.Applicative
import Control.Monad

import Backend.Mips

maxSingleApplication :: Int
maxSingleApplication = 2

-- putCharU :: MipsMonad ()
-- putCharU = do
--   putCharU_Entry <- global "_putCharU_Entry"

--   label putCharU_Entry

--   l   a0 (97 :: Int)
--   l   v0 (11 :: Int)
--   syscall
--   lw  t0 0 sp
--   j   t0

-- putChar :: MipsMonad ()
-- putChar = do
--   putCharU_Entry <- global "_putCharU_Entry"
--   putChar_Entry <- global "_putChar_Entry"
--   putChar_0 <- global "_putChar_0"

--   label putChar_Entry

--   sub sp sp (4 :: Int) -- Add continuation to _putChar_0 on top of the stack
--   l   t0 putChar_0
--   sw  t0 0 sp
--   lw  rt 4 rt          -- Evaluate argument
--   lw  t0 0 rt
--   j   t0

--   label putChar_0
--   add sp sp (4 :: Int)
--   j   putCharU_Entry

-- error :: MipsMonad ()
-- error = do
--   error_Entry <- global "_error_Entry"
--   error_0 <- global "_error_0"
--   error_1 <- global "_error_1"
--   putChar_Entry <- global "_putChar_Entry"

--   label error_Entry

--   sub sp sp (4 :: Int) -- Add continuation to error_0 on top of the stack
--   l   t0 error_0
--   sw  t0 0 sp
--   lw  rt 4 rt          -- Evaluate argument
--   lw  t0 0 rt
--   j   t0

--   label error_0        -- Call putChar#, then exit
--   l   t0 error_1       -- Add continuation to error_1 on top of the stack
--   sw  t0 0 sp
--   sub hp hp (8 :: Int)
--   l   t0 putChar_Entry
--   sw  t0 0 hp
--   sw  rt 4 hp
--   l   rt hp
--   j   putChar_Entry

--   label error_1        -- Exit
--   add sp sp (4 :: Int)
--   l   v0 (10 :: Int)
--   syscall
  
-- test_Main :: MipsMonad ()
-- test_Main = do
--   l   a0 (1024 :: Int)
--   l   v0 (9 :: Int)
--   syscall
--   l   hp v0
--   add hp hp (1024 :: Int)

--   error_Entry <- global "_error_Entry"
--   callError <- global "callError"

--   l   rt callError
--   j   error_Entry

-- aChar :: MipsMonad ()
-- aChar = do
--   aChar_Data <- global "aChar_Data"
--   nop_Closure <- global "nop_Closure"

--   label aChar_Data
--   word nop_Closure
--   word 'a'

-- callError :: MipsMonad ()
-- callError = do
--   callError <- global "callError"
--   error_Entry <- global "_error_Entry"
--   aChar_Data <- global "aChar_Data"

--   label callError
--   word error_Entry
--   word aChar_Data

continue :: SectionMonad ()
continue = do
  word (0 :: Int)
  continue <- global "_runtime_continue"
  label continue
  lw  v0 0 sp
  add sp sp (4 :: Int)
  j   v0

-- Print rt, then exit

exit :: SectionMonad ()
exit = do
  exit <- global "_runtime_exit"
  exit_continuation <- newLabel
  label exit
  -- Eval RT :
  sub sp sp (4 :: Int)
  l v0 exit_continuation
  sw v0 0 sp
  j =<< global "_runtime_apply_continuation_0"
  
  label exit_continuation
  l  v0 (1 :: Int)
  lw  a0 8 rt
  syscall
  l  v0 (10 :: Int)
  syscall

start :: SectionMonad ()
start = do
  start <- global "_runtime_start"
  continue <- global "_runtime_continue"
  exit <- global "_runtime_exit"
  idl <- global "_closure_Base_id45"
  apl <- global "_runtime_apply_continuation_1"
  label start
  
  sub sp sp (12 :: Int)
  l v0 exit
  sw v0 8 sp
  l v0 =<< global "_int_42"
  sw v0 4 sp
  l v0 apl
  sw v0 0 sp

  l rt idl

  -- Last continuation is exit

  j continue


apply_continuation_0 :: SectionMonad ()
apply_continuation_0 = do
  apply_continuation <- global "_runtime_apply_continuation_0"
  label apply_continuation

  lw v0 0 rt
  lw v1 (-4) v0

  -- Test if it is a function (already WHNF)
  bne v1 zero =<< global "_runtime_continue"
  -- Otherwise, eval (if it is an applied constructor in WHNF, it will call _runtime_continue)
  j v0

-- Function is in rt
-- Arguments on the stack
-- Function is in WHNF
apply_continuation :: Int -> SectionMonad ()
apply_continuation n = do
  apply_continuation <- global ("_runtime_apply_continuation_" ++ show n)
  apply_continuation_array <- global "_runtime_apply_continuation_array"
  pap_label          <- newLabel
  over_label         <- newLabel
  over_label_2       <- newLabel
  over_label_3       <- newLabel
  under_label        <- newLabel
  label apply_continuation

  lw v0 0 rt
  lw v1 (-4) v0

  -- Test if PAP (arity -1)
  bltz v1 pap_label
  -- Not PAP, arity = v1, needed arity = n
  sub a0 v1 n
  bltz a0 over_label
  bgtz a0 under_label
  -- Application with the right argument count
  j v0
  
  label over_label -- Too many arguments
  -- First eval with v1 args (v1 < n)
  sub sp sp (4 :: Int)
  l a1 (0 :: Int)
  forM_ [0..n-2] $ \_ -> do
    beq a1 v1 over_label_3
    lw a2 4 sp
    sw a2 0 sp
    add a1 a1 (1 :: Int)
    add sp sp (4 :: Int)
  label over_label_3
  -- Setup continuation (apply (n - v1))
  l a1 apply_continuation_array
  sub a1 a1 a0
  sub a1 a1 a0
  sub a1 a1 a0
  sub a1 a1 a0
  lw a1 0 a1
  sw a1 0 sp
  sub sp sp v1
  sub sp sp v1
  sub sp sp v1
  sub sp sp v1
  -- Call f
  j v0
  
  label under_label -- Not enough arguments
  -- Make a PAP with ENTRY = FAIL, CURRENT ARITY =, TOTAL ARITY 
  l v0 (-1 :: Int) -- Fail
  syscall
  -- PAP : ENTRY | CURRENT ARITY | TOTAL ARITY | FUNCTION | CURRENT ARGUMENTS
  label pap_label
  l v0 (-2 :: Int) -- Fail
  syscall
  
runtime :: MipsMonad ()
runtime = do
  textSection $ do
    continue
    exit
    start
    apply_continuation_0
    forM_ [1..maxSingleApplication] apply_continuation
  dataSection $ do
    label =<< global "_runtime_apply_continuation_array"
    forM_ [0..maxSingleApplication] $ \i ->
      word =<< global ("_runtime_apply_continuation_" ++ show i)

    label =<< global "_int_42"
    word =<< global "_runtime_continue"
    word (1 :: Int)
    word (42 :: Int)
