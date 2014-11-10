module Backend.Mips where

import Data.Monoid
import Control.Monad.Writer
import Control.Monad.State

import Data.Char (ord)

import Prelude hiding (div)

newtype MipsRegister = MipsRegister Int
                     deriving (Eq, Ord)

instance Show MipsRegister where
  show (MipsRegister i) = "$" ++ show i

zero, v0, v1, sp, hp, rt, a0, a1, a2, a3, t0, t1, t2, t3, t4, t5, t6, t7, s0, s1, s2, s3, s4, s5, s6, s7, t8, t9 :: MipsRegister
zero = MipsRegister 0

v0   = MipsRegister 2
v1   = MipsRegister 3

a0   = MipsRegister 4
a1   = MipsRegister 5
a2   = MipsRegister 6
a3   = MipsRegister 7

t0   = MipsRegister 8
t1   = MipsRegister 9
t2   = MipsRegister 10
t3   = MipsRegister 11
t4   = MipsRegister 12
t5   = MipsRegister 13
t6   = MipsRegister 14
t7   = MipsRegister 15

s0   = MipsRegister 16
s1   = MipsRegister 17
s2   = MipsRegister 18
s3   = MipsRegister 19
s4   = MipsRegister 20
s5   = MipsRegister 21
s6   = MipsRegister 22
s7   = MipsRegister 23

t8   = MipsRegister 24
t9   = MipsRegister 25

sp   = MipsRegister 29
hp   = MipsRegister 30

rt   = MipsRegister 31

newtype Address = Address (Either Int String)

instance Show Address where
  show (Address (Left i))  = "L" ++ show i
  show (Address (Right s)) = s

data MipsOperand = ORegister MipsRegister
                 | OConst Int
                 | OAddress Address

class Operand a where
  toOperand :: a -> MipsOperand

instance Operand MipsRegister where
  toOperand = ORegister

instance Operand Int where
  toOperand = OConst

instance Operand Char where
  toOperand = OConst . ord

instance Operand Address where
  toOperand = OAddress

data MipsInstruction = Label Address
                     | Word MipsOperand

                     | Add MipsRegister MipsRegister MipsOperand
                     | Sub MipsRegister MipsRegister MipsOperand
                     | Mul MipsRegister MipsRegister MipsRegister
                     | Div MipsRegister MipsRegister MipsRegister
                     | Rem MipsRegister MipsRegister MipsRegister

                     | L MipsRegister MipsOperand
                     | Lw MipsRegister Int MipsRegister
                     | Sw MipsRegister Int MipsRegister

                     | J MipsOperand
                     | Beq MipsRegister MipsRegister Address
                     | Bne MipsRegister MipsRegister Address
                     | Bltz MipsRegister Address
                     | Bgtz MipsRegister Address

                     | Seq MipsRegister MipsRegister MipsRegister
                     | Sne MipsRegister MipsRegister MipsRegister
                     | Slt MipsRegister MipsRegister MipsRegister
                     | Sle MipsRegister MipsRegister MipsRegister
                     | Sgt MipsRegister MipsRegister MipsRegister
                     | Sge MipsRegister MipsRegister MipsRegister

                     | Asciiz String

                     | Syscall

instance Show MipsInstruction where
  show (Label a)                  = show a ++ ":\n"
  show (Word (OConst c))          = "\t.word " ++ show c ++ "\n"
  show (Word (OAddress a))        = "\t.word " ++ show a ++ "\n"

  show (Add t a (ORegister b))    = "\tadd " ++ show t ++ ", " ++ show a ++ ", " ++ show b ++ "\n"
  show (Add t a (OConst c))       = "\taddi " ++ show t ++ ", " ++ show a ++ ", " ++ show c ++ "\n"
  show (Add t a (OAddress _))     = error "show mips instruction"
  show (Sub t a (ORegister b))    = "\tsub " ++ show t ++ ", " ++ show a ++ ", " ++ show b ++ "\n"
  show (Sub t a (OConst c))       = "\taddi " ++ show t ++ ", " ++ show a ++ ", " ++ show (-c) ++ "\n"
  show (Sub t a (OAddress _))     = error "show mips instruction"
  
  show (Mul t a b)                = "\tmul " ++ show t ++ ", " ++ show a ++ ", " ++ show b ++ "\n"
  show (Div t a b)                = "\tdiv " ++ show t ++ ", " ++ show a ++ ", " ++ show b ++ "\n"
  show (Rem t a b)                = "\trem " ++ show t ++ ", " ++ show a ++ ", " ++ show b ++ "\n"

  show (L r (ORegister r'))       = "\tmove " ++ show r ++ ", " ++ show r' ++ "\n"
  show (L r (OConst c))           = "\tli " ++ show r ++ ", " ++ show c ++ "\n"
  show (L r (OAddress a))         = "\tla " ++ show r ++ ", " ++ show a ++ "\n"
  show (Lw r c r')                = "\tlw " ++ show r ++ ", " ++ show c ++ "(" ++ show r' ++ ")\n"
  show (Sw r c r')                = "\tsw " ++ show r ++ ", " ++ show c ++ "(" ++ show r' ++ ")\n"

  show (J (ORegister r))          = "\tjr " ++ show r ++ "\n"
  show (J (OConst _))             = error "show mips instruction"
  show (J (OAddress a))           = "\tj " ++ show a ++ "\n"

  show (Beq r r' a)               = "\tbeq " ++ show r ++ ", " ++ show r' ++ ", " ++ show a ++ "\n"
  show (Bne r r' a)               = "\tbne " ++ show r ++ ", " ++ show r' ++ ", " ++ show a ++ "\n"
  show (Bltz r a)                 = "\tbltz " ++ show r ++ ", " ++ show a ++ "\n"
  show (Bgtz r a)                 = "\tbgtz " ++ show r ++ ", " ++ show a ++ "\n"
  
  show (Seq r a b)                = "\tseq " ++ show r ++ ", " ++ show a ++ ", " ++ show b ++ "\n"
  show (Sne r a b)                = "\tsne " ++ show r ++ ", " ++ show a ++ ", " ++ show b ++ "\n"
  show (Slt r a b)                = "\tslt " ++ show r ++ ", " ++ show a ++ ", " ++ show b ++ "\n"
  show (Sle r a b)                = "\tsle " ++ show r ++ ", " ++ show a ++ ", " ++ show b ++ "\n"
  show (Sgt r a b)                = "\tsgt " ++ show r ++ ", " ++ show a ++ ", " ++ show b ++ "\n"
  show (Sge r a b)                = "\tsge " ++ show r ++ ", " ++ show a ++ ", " ++ show b ++ "\n"

  show (Asciiz s)                 = "\t.asciiz " ++ show s ++ "\n"

  show Syscall                    = "\tsyscall\n"

data MipsInstructions = NOP
                      | Instruction MipsInstruction
                      | Instructions MipsInstructions MipsInstructions

instance Monoid MipsInstructions where
  mempty        = NOP
  mappend NOP a = a
  mappend a NOP = a
  mappend a b   = Instructions a b

instance Show MipsInstructions where
  show (Instruction i)    = show i
  show NOP                = ""
  show (Instructions a b) = show a ++ show b

data Mips = Mips
  { mipsText :: MipsInstructions
  , mipsData :: MipsInstructions
  }

instance Monoid Mips where
  mempty                                = Mips mempty mempty
  Mips txt dat `mappend` Mips txt' dat' = Mips (txt `mappend` txt') (dat `mappend` dat')

instance Show Mips where
  show (Mips txt dat) =
       "# TODO : Find a suitable header !\n"
    ++ "\t.text\n"
    ++ "main:\n"
    ++ show txt
    ++ "\t.data\n"
    ++ show dat

type MipsMonad a    = WriterT Mips (State Int) a
type SectionMonad a = WriterT MipsInstructions (State Int) a

runMips :: MipsMonad () -> Mips
runMips m = flip evalState 0 $ execWriterT m

newLabel :: SectionMonad Address
newLabel = do
  a <- get
  modify (+ 1)
  return $ Address (Left a)

textSection :: SectionMonad () -> MipsMonad ()
textSection m = do
  txt <- lift $ execWriterT m
  tell (Mips txt mempty)

dataSection :: SectionMonad () -> MipsMonad ()
dataSection m = do
  dat <- lift $ execWriterT m
  tell (Mips mempty dat)

global :: String -> SectionMonad Address
global s = return (Address (Right s))

label :: Address -> SectionMonad ()
label = tell . Instruction . Label

word :: Operand o => o -> SectionMonad ()
word a = tell . Instruction $ Word (toOperand a)

add :: Operand o => MipsRegister -> MipsRegister -> o -> SectionMonad ()
add t a b = tell . Instruction $ Add t a (toOperand b)

sub :: Operand o => MipsRegister -> MipsRegister -> o -> SectionMonad ()
sub t a b = tell . Instruction $ Sub t a (toOperand b)

mul :: MipsRegister -> MipsRegister -> MipsRegister -> SectionMonad ()
mul t a b = tell . Instruction $ Mul t a b

div :: MipsRegister -> MipsRegister -> MipsRegister -> SectionMonad ()
div t a b = tell . Instruction $ Div t a b

rem :: MipsRegister -> MipsRegister -> MipsRegister -> SectionMonad ()
rem t a b = tell . Instruction $ Rem t a b

l :: Operand o => MipsRegister -> o -> SectionMonad ()
l t a = tell . Instruction $ L t (toOperand a)

li :: MipsRegister -> Int -> SectionMonad ()
li t a = tell . Instruction $ L t (OConst a)

lw :: MipsRegister -> Int -> MipsRegister -> SectionMonad ()
lw a c b = tell . Instruction $ Lw a c b

sw :: MipsRegister -> Int -> MipsRegister -> SectionMonad ()
sw a c b = tell . Instruction $ Sw a c b

j :: Operand o => o -> SectionMonad ()
j a = tell . Instruction $ J (toOperand a)

beq :: MipsRegister -> MipsRegister -> Address -> SectionMonad ()
beq a b c = tell . Instruction $ Beq a b c

bne :: MipsRegister -> MipsRegister -> Address -> SectionMonad ()
bne a b c = tell . Instruction $ Bne a b c

bltz :: MipsRegister -> Address -> SectionMonad ()
bltz a b = tell . Instruction $ Bltz a b

bgtz :: MipsRegister -> Address -> SectionMonad ()
bgtz a b = tell . Instruction $ Bgtz a b

seq :: MipsRegister -> MipsRegister -> MipsRegister -> SectionMonad ()
seq r a b = tell . Instruction $ Seq r a b

sne :: MipsRegister -> MipsRegister -> MipsRegister -> SectionMonad ()
sne r a b = tell . Instruction $ Sne r a b

slt :: MipsRegister -> MipsRegister -> MipsRegister -> SectionMonad ()
slt r a b = tell . Instruction $ Slt r a b

sle :: MipsRegister -> MipsRegister -> MipsRegister -> SectionMonad ()
sle r a b = tell . Instruction $ Sle r a b

sgt :: MipsRegister -> MipsRegister -> MipsRegister -> SectionMonad ()
sgt r a b = tell . Instruction $ Sgt r a b

sge :: MipsRegister -> MipsRegister -> MipsRegister -> SectionMonad ()
sge r a b = tell . Instruction $ Sge r a b

asciiz :: String -> SectionMonad ()
asciiz s = tell . Instruction $ Asciiz s

syscall :: SectionMonad ()
syscall = (tell . Instruction) Syscall

tst :: SectionMonad ()
tst = do
  li v0 10
  syscall
