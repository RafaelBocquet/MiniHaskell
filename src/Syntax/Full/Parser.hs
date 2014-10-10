{-# OPTIONS_GHC -w #-}
module Syntax.Full.Parser where

import Syntax.Full.Token
import Control.Applicative(Applicative(..))

-- parser produced by Happy Version 1.19.4

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18

action_0 (41) = happyShift action_3
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_2
action_0 _ = happyReduce_3

action_1 (41) = happyShift action_3
action_1 (5) = happyGoto action_2
action_1 _ = happyFail

action_2 (66) = happyShift action_8
action_2 (6) = happyGoto action_7
action_2 _ = happyFail

action_3 (20) = happyShift action_6
action_3 (7) = happyGoto action_5
action_3 _ = happyFail

action_4 (68) = happyAccept
action_4 _ = happyFail

action_5 (46) = happyShift action_28
action_5 _ = happyFail

action_6 _ = happyReduce_7

action_7 _ = happyReduce_1

action_8 (19) = happyShift action_17
action_8 (27) = happyShift action_18
action_8 (28) = happyShift action_19
action_8 (29) = happyShift action_20
action_8 (34) = happyShift action_21
action_8 (36) = happyShift action_22
action_8 (37) = happyShift action_23
action_8 (38) = happyShift action_24
action_8 (39) = happyShift action_25
action_8 (42) = happyShift action_26
action_8 (45) = happyShift action_27
action_8 (8) = happyGoto action_9
action_8 (9) = happyGoto action_10
action_8 (10) = happyGoto action_11
action_8 (11) = happyGoto action_12
action_8 (12) = happyGoto action_13
action_8 (15) = happyGoto action_14
action_8 (16) = happyGoto action_15
action_8 (17) = happyGoto action_16
action_8 _ = happyFail

action_9 _ = happyReduce_26

action_10 _ = happyReduce_28

action_11 _ = happyReduce_15

action_12 _ = happyReduce_16

action_13 (23) = happyShift action_39
action_13 (14) = happyGoto action_38
action_13 _ = happyReduce_24

action_14 (19) = happyShift action_17
action_14 (27) = happyShift action_18
action_14 (28) = happyShift action_19
action_14 (29) = happyShift action_20
action_14 (36) = happyShift action_22
action_14 (37) = happyShift action_23
action_14 (38) = happyShift action_24
action_14 (39) = happyShift action_25
action_14 (42) = happyShift action_26
action_14 (45) = happyShift action_27
action_14 (67) = happyShift action_37
action_14 (9) = happyGoto action_10
action_14 (10) = happyGoto action_11
action_14 (11) = happyGoto action_12
action_14 (12) = happyGoto action_13
action_14 (13) = happyGoto action_35
action_14 (16) = happyGoto action_36
action_14 (17) = happyGoto action_16
action_14 (18) = happyGoto action_33
action_14 _ = happyReduce_32

action_15 (67) = happyShift action_34
action_15 (13) = happyGoto action_32
action_15 (18) = happyGoto action_33
action_15 _ = happyReduce_32

action_16 (50) = happyShift action_30
action_16 (61) = happyShift action_31
action_16 _ = happyFail

action_17 _ = happyReduce_30

action_18 _ = happyReduce_12

action_19 _ = happyReduce_10

action_20 _ = happyReduce_14

action_21 (20) = happyShift action_6
action_21 (7) = happyGoto action_29
action_21 _ = happyFail

action_22 _ = happyReduce_21

action_23 _ = happyReduce_19

action_24 _ = happyReduce_20

action_25 _ = happyReduce_13

action_26 _ = happyReduce_11

action_27 _ = happyReduce_9

action_28 _ = happyReduce_2

action_29 _ = happyReduce_8

action_30 _ = happyReduce_18

action_31 (19) = happyShift action_45
action_31 _ = happyFail

action_32 (19) = happyShift action_17
action_32 (27) = happyShift action_18
action_32 (28) = happyShift action_19
action_32 (29) = happyShift action_20
action_32 (36) = happyShift action_22
action_32 (37) = happyShift action_23
action_32 (38) = happyShift action_24
action_32 (39) = happyShift action_25
action_32 (42) = happyShift action_26
action_32 (45) = happyShift action_27
action_32 (9) = happyGoto action_44
action_32 (10) = happyGoto action_11
action_32 (11) = happyGoto action_12
action_32 (12) = happyGoto action_13
action_32 (17) = happyGoto action_16
action_32 _ = happyFail

action_33 (62) = happyShift action_43
action_33 _ = happyFail

action_34 _ = happyReduce_6

action_35 (34) = happyShift action_21
action_35 (8) = happyGoto action_42
action_35 _ = happyFail

action_36 (67) = happyShift action_41
action_36 (13) = happyGoto action_32
action_36 (18) = happyGoto action_33
action_36 _ = happyReduce_32

action_37 _ = happyReduce_4

action_38 (21) = happyShift action_40
action_38 _ = happyFail

action_39 _ = happyReduce_23

action_40 _ = happyReduce_17

action_41 _ = happyReduce_5

action_42 _ = happyReduce_25

action_43 (62) = happyReduce_31
action_43 _ = happyReduce_22

action_44 _ = happyReduce_27

action_45 _ = happyReduce_29

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 _
	_
	 =  HappyAbsSyn4
		 (0
	)

happyReduce_2 = happySpecReduce_3  5 happyReduction_2
happyReduction_2 _
	_
	_
	 =  HappyAbsSyn5
		 (0
	)

happyReduce_3 = happySpecReduce_0  5 happyReduction_3
happyReduction_3  =  HappyAbsSyn5
		 (0
	)

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 _
	_
	_
	 =  HappyAbsSyn6
		 (0
	)

happyReduce_5 = happyReduce 4 6 happyReduction_5
happyReduction_5 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (0
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_3  6 happyReduction_6
happyReduction_6 _
	_
	_
	 =  HappyAbsSyn6
		 (0
	)

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 _
	 =  HappyAbsSyn7
		 (0
	)

happyReduce_8 = happySpecReduce_2  8 happyReduction_8
happyReduction_8 _
	_
	 =  HappyAbsSyn8
		 (0
	)

happyReduce_9 = happySpecReduce_1  9 happyReduction_9
happyReduction_9 _
	 =  HappyAbsSyn9
		 (0
	)

happyReduce_10 = happySpecReduce_1  9 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn9
		 (0
	)

happyReduce_11 = happySpecReduce_1  9 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn9
		 (0
	)

happyReduce_12 = happySpecReduce_1  9 happyReduction_12
happyReduction_12 _
	 =  HappyAbsSyn9
		 (0
	)

happyReduce_13 = happySpecReduce_1  9 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn9
		 (0
	)

happyReduce_14 = happySpecReduce_1  9 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn9
		 (0
	)

happyReduce_15 = happySpecReduce_1  9 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn9
		 (0
	)

happyReduce_16 = happySpecReduce_1  10 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn10
		 (0
	)

happyReduce_17 = happySpecReduce_3  11 happyReduction_17
happyReduction_17 _
	_
	_
	 =  HappyAbsSyn11
		 (0
	)

happyReduce_18 = happySpecReduce_2  11 happyReduction_18
happyReduction_18 _
	_
	 =  HappyAbsSyn11
		 (0
	)

happyReduce_19 = happySpecReduce_1  12 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn12
		 (0
	)

happyReduce_20 = happySpecReduce_1  12 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn12
		 (0
	)

happyReduce_21 = happySpecReduce_1  12 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn12
		 (0
	)

happyReduce_22 = happySpecReduce_2  13 happyReduction_22
happyReduction_22 (HappyTerminal happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_2 : happy_var_1
	)
happyReduction_22 _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  14 happyReduction_23
happyReduction_23 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (Just happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_0  14 happyReduction_24
happyReduction_24  =  HappyAbsSyn14
		 (Nothing
	)

happyReduce_25 = happySpecReduce_3  15 happyReduction_25
happyReduction_25 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn15
		 (happy_var_3 : happy_var_1
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  15 happyReduction_26
happyReduction_26 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn15
		 ([happy_var_1]
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  16 happyReduction_27
happyReduction_27 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_3 : happy_var_1
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  16 happyReduction_28
happyReduction_28 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn16
		 ([happy_var_1]
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  17 happyReduction_29
happyReduction_29 (HappyTerminal happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_3 : happy_var_1
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  17 happyReduction_30
happyReduction_30 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn17
		 ([happy_var_1]
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_2  18 happyReduction_31
happyReduction_31 (HappyTerminal happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_2 : happy_var_1
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_0  18 happyReduction_32
happyReduction_32  =  HappyAbsSyn18
		 ([]
	)

happyNewToken action sts stk [] =
	action 68 68 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	t | isVariableIdentifier t -> cont 19;
	t | isConstructorIdentifier t -> cont 20;
	t | isSymbolIdentifier t -> cont 21;
	t | isConstructorSymbolIdentifier t -> cont 22;
	TkInteger _ -> cont 23;
	TkChar _ -> cont 24;
	TkString _ -> cont 25;
	TkCase -> cont 26;
	TkClass -> cont 27;
	TkData -> cont 28;
	TkDefault -> cont 29;
	TkDeriving -> cont 30;
	TkDo -> cont 31;
	TkElse -> cont 32;
	TkIf -> cont 33;
	TkImport -> cont 34;
	TkIn -> cont 35;
	TkInfix -> cont 36;
	TkInfixl -> cont 37;
	TkInfixr -> cont 38;
	TkInstance -> cont 39;
	TkLet -> cont 40;
	TkModule -> cont 41;
	TkNewtype -> cont 42;
	TkOf -> cont 43;
	TkThen -> cont 44;
	TkType -> cont 45;
	TkWhere -> cont 46;
	TkUnderscore -> cont 47;
	TkDoubleDot -> cont 48;
	TkColon -> cont 49;
	TkDoubleColon -> cont 50;
	TkEqual -> cont 51;
	TkLambda -> cont 52;
	TkPipe -> cont 53;
	TkLArrow -> cont 54;
	TkRArrow -> cont 55;
	TkAt -> cont 56;
	TkTilde -> cont 57;
	TkFatArrow -> cont 58;
	TkLParen -> cont 59;
	TkRParen -> cont 60;
	TkComma -> cont 61;
	TkSemiColon -> cont 62;
	TkLBracket -> cont 63;
	TkRBracket -> cont 64;
	TkBackTick -> cont 65;
	TkLBrace -> cont 66;
	TkRBrace -> cont 67;
	_ -> happyError' (tk:tks)
	}

happyError_ 68 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure    = return
    a <*> b = (fmap id a) <*> b
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq



{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
