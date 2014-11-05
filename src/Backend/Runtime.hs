module Backend.Runtime where

import Prelude hiding (error, putChar)

import Control.Applicative
import Control.Monad

import Backend.Mips

maxSingleApplication :: Int
maxSingleApplication = 3

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
  idl <- global "_closure_Base_id"
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

pap :: SectionMonad ()
pap = do
  word (-1 :: Int)
  label =<< global "_runtime_pap"
  l v0 (-10 :: Int)
  syscall 

apply_continuation_0 :: SectionMonad ()
apply_continuation_0 = do
  label =<< global "_runtime_apply_continuation_0"

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
  pap_over_label        <- newLabel
  pap_over_label_2        <- newLabel
  pap_under_label        <- newLabel
  label apply_continuation

  lw a3 0 rt
  lw v1 (-4) a3

  -- Test if PAP (arity -1)
  bltz v1 pap_label
  -- Not PAP, arity = v1, needed arity = n
  sub a2 v1 n
  bltz a2 over_label
  bgtz a2 under_label
  -- Application with the right argument count
  j a3
  
  label over_label -- Too many arguments
  -- First eval with v1 args (v1 < n)
  sub sp sp (4 :: Int)
  l a1 (0 :: Int)
  forM_ [0..n-2] $ \_ -> do
    beq a1 v1 over_label_3
    lw a0 4 sp
    sw a0 0 sp
    add a1 a1 (1 :: Int)
    add sp sp (4 :: Int)
  label over_label_3
  -- Setup continuation (apply (n - v1))
  l a1 apply_continuation_array
  sub a1 a1 a2
  sub a1 a1 a2
  sub a1 a1 a2
  sub a1 a1 a2
  lw a1 0 a1
  sw a1 0 sp
  sub sp sp v1
  sub sp sp v1
  sub sp sp v1
  sub sp sp v1
  -- Call f
  j a3
  
  label under_label -- Not enough arguments
  l v0 (-1 :: Int)
  syscall
  -- Make a PAP with ENTRY = FAIL, CURRENT (REMAINING) ARITY = a2, 4*NVARS = 4*n, vars
  -- Need 4 * (3 + n) Heap memory
  l a0 (4 * (4 + n))
  l v0 (9 :: Int)
  syscall
  -- Memory is pointed by v0
  l  a0 =<< global "_runtime_pap"
  sw a0 0 v0
  sw a2 4 v0
  l  a0 (4 * n)
  sw a0 8 v0
  sw a3 12 v0
  forM [0..n-1] $ \i -> do
    lw a0 (4*i) sp
    sw a0 (16+4*i) v0
  add sp sp (4 * n)
  l rt v0
  j =<< global "_runtime_continue"
  
  -- PAP : ENTRY | CURRENT ARITY | 4*NVARS | FUNCTION | CURRENT ARGUMENTS
  -- n args on stack
  -- cases :
  --  * n = CUR ARITY
  --  * n < CUR ARITY
  --  * n > CUR ARITY
  label pap_label
  lw   v1 4 rt
  sub  a2 v1 n
  bgtz a2 pap_under_label

  -- -- Either : Exact case : increment stack with all remaining arguments, and call f
  -- --          Over case  : idem, then call apply_continuation with less args
  lw  a0 8  rt
  add v0 rt (16 :: Int)
  add v0 v0 a0
  
  label pap_over_label
  beq a0 zero pap_over_label_2
  sub a0 a0 (4 :: Int)
  sub sp sp (4 :: Int)
  sub v0 v0 (4 :: Int)
  lw  a1 0 v0
  sw  a1 0 sp
  j   pap_over_label
  label pap_over_label_2

  -- -- Call the function
  lw a0 12 rt
  j a0

  label pap_under_label
  l v0 (-11 :: Int)
  syscall

  
runtime :: MipsMonad ()
runtime = do
  textSection $ do
    continue
    exit
    start
    pap
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
