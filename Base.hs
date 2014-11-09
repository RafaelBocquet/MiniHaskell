module Base where {

; import Primitive

-- Int

; data Int = Int Int_prim

; (+), (-), (*), div :: Int -> Int -> Int
; (+) (Int a) (Int b) = add_prim a b
; (-) (Int a) (Int b) = sub_prim a b
; (*) (Int a) (Int b) = mul_prim a b
; div (Int a) (Int b) = div_prim a b
; rem (Int a) (Int b) = rem_prim a b

; negate :: Int -> Int
; negate (Int a) = negate_prim a

-- ; (<), (<=), (>), (>=), (==), (/=) :: Int -> Int -> Bool
-- ; (<)  (Int a) (Int b) = intLT_prim a b
-- ; (<=) (Int a) (Int b)  = intLE_prim a b
-- ; (>)  (Int a) (Int b) = intGT_prim a b
-- ; (>=) (Int a) (Int b)  = intGE_prim a b
-- ; (==) (Int a) (Int b)  = intEQ_prim a b
-- ; (/=) (Int a) (Int b)  = intNE_prim a b

; fromInteger :: Int -> Int
; fromInteger x = x

-- ; error x = error x

-- ; (>>=) :: IO a -> (a -> IO b) -> IO b
-- ; (>>=) a b = error a

-- ; putChar :: Char -> IO Char
-- ; putChar c = error c

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

; head :: [a] -> a
; head (x : _) = x

; tail :: [a] -> [a]
; tail (_ : xs) = xs

; foldr :: (a -> b -> b) -> b -> [a] -> b
; foldr f x []       = x
; foldr f x (y : ys) = f y (foldr f x ys)

; foldl :: (b -> a -> b) -> b -> [a] -> b
; foldl f x []       = x
; foldl f x (y : ys) = foldl f (f x y) ys

; sum :: [Int] -> Int
; sum xs = foldr (+) 0 xs

; product :: [Int] -> Int
; product xs = foldl (*) 1 xs

-- Any
; id :: Int -> Int
; id x = product (1 : (2 : (3 : (4 : []))))

-- ; id'' x = id' id' x

-- ; id' x = x

-- ; fst (x, _) = x
-- ; snd (_, y) = y


-- ; (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- ; (.) f g x = f (g x)

-- ; const :: a -> b -> a
-- ; const x _ = x

-- ; ap :: (a -> b) -> a -> b
-- ; ap f x = f x

-- ; undefined :: a
-- ; undefined = error "undefined"

-- Bool

-- ; not :: Bool -> Bool
-- ; not True  = False
-- ; not False = True

; (&&), (||) :: Bool -> Bool -> Bool
; (&&) True  x = x
; (&&) False x = False

; (||) True  _ = True
; (||) False x = x

-- List

}
