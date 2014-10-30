module Base where {

; import Primitive

-- Int

; data Int = Int Int_prim

-- ; (+), (-), (*), div, rem :: Int -> Int -> Int
-- ; (+) (Int a) (Int b) = Int (add_prim a b)
-- ; (-) (Int a) (Int b) = Int (sub_prim a b)
-- ; (*) (Int a) (Int b) = Int (mul_prim a b)
-- ; div (Int a) (Int b) = Int (div_prim a b)
-- ; rem (Int a) (Int b) = Int (rem_prim a b)

-- ; negate :: Int -> Int
-- ; negate (Int a) = Int (negate_prim a)

-- ; (<), (<=), (>), (>=), (==), (/=) :: Int -> Int -> Bool
-- ; (<)  (Int a) (Int b) = intLT_prim a b
-- ; (<=) (Int a) (Int b)  = intLE_prim a b
-- ; (>)  (Int a) (Int b) = intGT_prim a b
-- ; (>=) (Int a) (Int b)  = intGE_prim a b
-- ; (==) (Int a) (Int b)  = intEQ_prim a b
-- ; (/=) (Int a) (Int b)  = intNE_prim a b

-- ; fromInteger x = x

-- Char

-- ; data Char = Char Char_prim

-- ; chr (Int a) = Char (chr_prim a)
-- ; ord (Char a) = Int (ord_prim a)

-- Type class
--; class Eq Char where {
--  ; (==) (Char a) (Char b) = charEQ_prim a b
--  ; (/=) (Char a) (Char b) = charNE_prim a b
--; }
--; class Ord Char where
--  ; (<)  (Char a) (Char b) = charLT_prim a b
--  ; (<=) (Char a) (Char b) = charLE_prim a b
--  ; (>)  (Char a) (Char b) = charGT_prim a b
--  ; (>=) (Char a) (Char b) = charGE_prim a b
--; }

-- Any
; id :: a -> a
; id x = id' (id' (id' (id' (id' x))))

; id' x = x


-- ; (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- ; (.) f g x = f (g x)

; const :: a -> b -> a
; const x _ = x

-- ; undefined :: a
-- ; undefined = error "undefined"

-- Bool

-- ; not :: Bool -> Bool
-- ; not True  = False
-- ; not False = True

-- ; (&&), (||) :: Bool -> Bool -> Bool
-- ; (&&) True  = \x -> x
-- ; (&&) False = \_ -> False

-- ; (||) True  = \_ -> True
-- ; (||) False = \x -> x

-- List

-- ; append :: List a -> List a -> List a
-- ; append []       ys = ys
-- ; append (x : xs) ys = x : append xs ys
}
