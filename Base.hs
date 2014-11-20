module Base where

import Primitive

-- Int

data Int = Int Int_prim

(+), (-), (*), div :: Int -> Int -> Int
(+) (Int a) (Int b) = add_prim a b
(-) (Int a) (Int b) = sub_prim a b
(*) (Int a) (Int b) = mul_prim a b
div (Int a) (Int b) = div_prim a b
rem (Int a) (Int b) = rem_prim a b

negate :: Int -> Int
negate (Int a) = negate_prim a

(<), (<=), (>), (>=), (==), (/=) :: Int -> Int -> Bool
(<)  (Int a) (Int b) = intLT_prim a b
(<=) (Int a) (Int b) = intLE_prim a b
(>)  (Int a) (Int b) = intGT_prim a b
(>=) (Int a) (Int b) = intGE_prim a b
(==) (Int a) (Int b) = intEQ_prim a b
(/=) (Int a) (Int b) = intNE_prim a b

error :: [Char] -> a
error x = error x

-- ; (>>=) :: IO a -> (a -> IO b) -> IO b
-- ; (>>=) a b = error a

-- ; putChar :: Char -> IO Char
-- ; putChar c = error c

-- Char

data Char = Char Char_prim

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

-- Maybe

data Maybe a = Nothing | Just a

-- Either

data Either a b = Left a | Right b

-- Ordering

data Ordering = LT | EQ | GT

-- IO

return = return_io_prim
putChar (Char c) = putChar_prim c
(>>=) a f = bind_io_prim a f

-- Other

-- ; sum :: [Int] -> Int
-- ; sum xs = foldl (+) 0 xs

if' :: a -> a -> Bool -> a
if' a b True  = a
if' a b False = b

count :: Int -> Int -> Int -> [Int]
count f t s = if' (f : []) (f : count (f + s) t s) (f == t)

-- ; product :: [Int] -> Int
-- ; product xs = foldl (*) 1 xs

-- Any

id :: Int -> Int
id x = head (map (\x -> x + 1) (x : []))

fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, y) = y

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) f g x = f (g x)

const :: a -> b -> a
const x _ = x

($) :: (a -> b) -> a -> b
($) f x = f x

-- ; undefined :: a
-- ; undefined = error "undefined"

-- Bool

not :: Bool -> Bool
not True  = False
not False = True

(&&), (||) :: Bool -> Bool -> Bool
(&&) True  x = x
(&&) False x = False

(||) True  _ = True
(||) False x = x

-- List

map :: (a -> b) -> [a] -> [b]
map f []       = []
map f (x : xs) = f x : map f xs

singleton :: a -> [a]
singleton x = x : []

(++) :: [a] -> [a] -> [a]
(++) [] ys       = ys
(++) (x : xs) ys = x : (xs ++ ys)

filter :: (a -> Bool) -> [a] -> [a]
filter f [] = []
filter f (x : xs) = case f x of
  True  -> x : filter f xs
  False -> filter f xs

head :: [a] -> a
head (x : _) = x

tail :: [a] -> [a]
tail (_ : xs) = xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f x []       = x
foldr f x (y : ys) = f y (foldr f x ys)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f x []       = x
foldl f x (y : ys) = foldl f (f x y) ys

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

repeat :: a -> [a]
repeat x = x : repeat x

cycle :: [a] -> [a]
cycle xs = xs ++ cycle xs

zip :: [a] -> [b] -> [(a, b)]
zip [] _ = []
zip _ [] = []
zip (x : xs) (y : ys) = (x, y) : zip xs ys

-- ; Typechecking loop ...
-- ; unzip :: [(a, b)] -> ([a], [b])
-- ; unzip l = foldr (\(a, b) (as, bs) -> (a : as, b : bs)) ([], []) l
