------------------------------------------------------------------
-- note:
--    foldl takes (f acc elt)
---   foldr takes (f elt acc)
------------------------------------------------------------------

all :: (a -> Bool) -> [a] -> Bool

-- test with
-- *Main> Main.all (==1) [1, 1, 1]
-- True
-- *Main> Main.all (==1) [1, 1, 2]
-- False

all p xs = and (Prelude.map p xs)                  -- yes
-- all p xs = Prelude.map p (and xs)               -- no
-- all p = and . Prelude.map p                     -- yes
-- all p = not . any (not . p)                     -- yes
-- all p = Prelude.map p . and                     -- no
-- all p xs = foldl (&&) True (Prelude.map p xs)   -- yes
-- all p xs = foldr (&&) False (Prelude.map p xs)  -- compiles, but doesn't do what you want
-- all p = foldr (&&) True . Prelude.map p         -- yes

------------------------------------------------------------------

any :: (a -> Bool) -> [a] -> Bool


-- test with
-- *Main> Main.any (==1) [1, 1, 2]
-- True
-- *Main> Main.any (==1) [2, 2, 2]
-- False


-- any p = Prelude.map p . or                          -- no
any p = or . Prelude.map p                             -- yes
-- any p xs = length (filter p xs) > 0                 -- yes
-- any p = not . null . dropWhile (not . p)            -- yes
-- any p = null . filter p                             -- compiles but does the wrong thing
-- any p xs = not (Main.all (\ x -> not (p x)) xs)     -- yes
-- any p xs = foldr(\ x acc -> (p x) || acc) False xs  -- yes
-- any p xs = foldr (||) True (Prelude.map p xs)       -- compiles but does the wrong thing

------------------------------------------------------------------

takeWhile :: (a -> Bool) -> [a] -> [a]

-- test with
-- *Main> Main.takeWhile (==1) [2, 2, 2]
-- []
-- *Main> Main.takeWhile (==1) [1, 2, 2]
-- [1]
-- *Main> Main.takeWhile (==1) [1..]
-- [1]
-- *Main> Main.takeWhile (==1) [1, 2, 1]
-- [1]


-- no:
-- takeWhile _ [] = []
-- takeWhile p (x : xs)
--   | p x = x : Main.takeWhile p xs
--   | otherwise = Main.takeWhile p xs

-- yes:
takeWhile _ [] = []
takeWhile p (x : xs)
  | p x = x : Main.takeWhile p xs
  | otherwise = []

-- no:
-- takeWhile _ [] = []
-- takeWhile p (x : xs)
--   | p x = Main.takeWhile p xs
--   | otherwise = []

-- no -- this seems to do the right thing, but it doesn't stop early,
-- so it doesn't work on [1, 2, 1]
--
-- takeWhile p = foldl (\ acc x -> if p x then x : acc else acc) []

------------------------------------------------------------------

dropWhile :: (a -> Bool) -> [a] -> [a]

-- test with
-- *Main> Main.dropWhile (==1) [2, 2, 2]
-- [2, 2, 2]
-- *Main> Main.dropWhile (==1) [1, 2, 2]
-- [2, 2]
-- *Main> Main.dropWhile (==1) [1, 2, 1]
-- [2, 1]
-- *Main> Main.dropWhile (==1) [1, 1, 1]
-- []

-- yes:
dropWhile _ [] = []
dropWhile p (x : xs)
  | p x = Main.dropWhile p xs
  | otherwise = x : xs

-- no:
-- dropWhile _ [] = []
-- dropWhile p (x : xs)
--   | p x = Main.dropWhile p xs
--   | otherwise = xs

-- no, doesn't stop early, it's basically like filter
-- dropWhile p = foldr (\ x acc -> if p x then acc else x : acc) []

-- no, it reverses the output
-- dropWhile p = foldl add []
--   where add [] x = if p x then [] else [x]
--         add acc x = x : acc

------------------------------------------------------------------

map :: (a -> b) -> [a] -> [b]

-- test with
-- *Main> Main.map (+1) [1, 2, 3]
-- [2,3,4]

-- map f = foldr (\ x xs -> xs ++ [f x]) []   -- no, this is backwards
-- map f = foldr (\ x xs -> f x ++ xs) []     -- no can't ++ with f x, which isn't a list
-- map f = foldl (\ xs x -> f x : xs) []      -- no, this is backwards
map f = foldl (\ xs x -> xs ++ [f x]) []      -- yes

------------------------------------------------------------------

filter :: (a -> Bool) -> [a] -> [a]

-- test with
-- *Main> Main.filter (not . (==1)) [1, 2, 1, 3]
-- [2,3]

-- filter p = foldl (\ xs x -> if p x then x : xs else xs) []     -- backwards
filter p = foldr (\ x xs -> if p x then x : xs else xs) []     -- yes
-- filter p = foldr (\ x xs -> if p x then xs ++ [x] else xs) []  -- backwards
-- filter p = foldl (\ x xs -> if p x then xs ++ [x] else xs) []  -- invalid
-- filter p = foldl (\ xs x -> if p x then xs ++ [x] else xs) []  -- this seems to work too, but it's not an option (I added it)

------------------------------------------------------------------

dec2int :: [Integer] -> Integer

-- test with
-- dec2int [2, 3, 4, 5]
-- 2345
-- dec2int []
-- 0
-- dec2int [0, 0, 0, 0]
-- 0
-- dec2int [0, 3, 0, 1]
-- 301

-- dec2int = foldr (\ x y -> 10 * x + y) 0 -- switches the arguments
-- dec2int = foldl (\ x y -> x + 10 * y) 0
dec2int = foldl (\ x y -> 10 * x + y) 0  -- switches the arguments
-- dec2int = foldr (\ x y -> x + 10 * y) 0 -- backwards

------------------------------------------------------------------

-- Invalid because sum is not a function from a -> a, it is a function from [a] -> a
--
-- sumsqreven = compose [sum, Prelude.map (^ 2), Prelude.filter even]

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

------------------------------------------------------------------

curry :: ((a, b) -> c) -> a -> b -> c

-- takes a function that takes its arguments as a pair and transforms
-- it into a function that takes its arguments one at a time

-- curry f = \ x y -> f x y
-- curry f = \ x y -> f
curry f = \ x y -> f (x, y)
-- curry f = \ (x, y) -> f x y

------------------------------------------------------------------

uncurry :: (a -> b -> c) -> (a, b) -> c

-- converts a function that takes its arguments one at a time into a
-- function that takes its arguments as a pair

uncurry f = \ (x, y) -> f x y
-- uncurry f = \ x y -> f (x, y)
-- uncurry f = \ (x, y) -> f
-- uncurry f = \ x y -> f

------------------------------------------------------------------

unfold :: (b -> Bool) -> (b -> a) -> (b -> b) -> b -> [a]
unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

-- produces empty list if p x is True
-- otherwise, applies (h x) to the head and (t x) to generate another seed

-- Example of int2bin
--
-- takes a non-negative integer into a binary number with least
-- significant bit first
--
type Bit = Int
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- int2bin 13
-- [1, 0, 1, 1]
-- int2bin (-0) -- ok for 0 to be negative
-- []

-- could have been written:
-- int2bin = unfold (== 0) (`mod` 2) (`div` 2)

-- chop8 takes a list of bits and chops it into lists of at most 8 bits
chop8 :: [Bit] -> [[Bit]]
-- chop8 [] = []
-- chop8 bits = take 8 bits : chop8 (drop 8 bits)

-- could have been:

-- chop8 = unfold [] (drop 8) (take 8)
chop8 = unfold null (take 8) (drop 8)
-- chop8 = unfold null (drop 8) (take 8)
-- chop8 = unfold (const False) (take 8) (drop 8)

------------------------------------------------------------------

-- implement map with unfold

mapu :: (a -> b) -> [a] -> [b]

-- mapu f = unfold null (f) tail
-- mapu f = unfold null (f (head)) tail
mapu f = unfold null (f . head) tail
-- mapu f = unfold empty (f . head) tail

------------------------------------------------------------------

-- implement iterate using unfold

iterate :: (a -> a) -> a -> [a]

iterate f = unfold (const False) id f
-- iterate f = unfold (const False) f f
-- iterate f = unfold (const True) id f
-- iterate f = unfold (const True) id f

------------------------------------------------------------------
