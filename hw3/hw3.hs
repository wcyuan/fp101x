-------------------------------------------------------------------
-- halve takes an even length lists and makes it into two equal length lists

-- doesn't work, can't use / on an integer
-- halve1 xs = (take n xs, drop n xs)
--   where n = length xs / 2

halve2 xs = splitAt (length xs `div` 2) xs

halve3 xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
  where n = length xs

-- doesn't work, needs xs at the end
halve4 xs = splitAt (length xs `div` 2)

-- doesn't work, drops the first element after the halfway
halve5 xs = (take n xs, drop (n + 1) xs)
  where n = length xs `div` 2

halve6 xs = splitAt (div (length xs) 2) xs

-- doesn't work, can't use / on an integer
-- halve7 xs = splitAt (length xs / 2) xs

halve8 xs = (take n xs, drop n xs)
  where n = length xs `div` 2

-------------------------------------------------------------------
-- safetail is just like tail except it maps the empty list to the empty list
-- insetad of throwing an error

safetail1 xs = if null xs then [] else tail xs

safetail2 [] = []
safetail2 (_ : xs) = xs

-- fails on empty: non-exhaustive pattern
-- even when not empty, skips the first two, not just the first one
safetail3 (_ : xs)
  | null xs = []
  | otherwise = tail xs

safetail4 xs
  | null xs = []
  | otherwise = tail xs

-- doesn't work, patterns are matched in order
-- this gives a compiler error when loading the file too
-- safetail5 xs = tail xs
-- safetail5 [] = []

safetail6 [] = []
safetail6 xs = tail xs

-- doesn't work on empty.  also, wrong return value when there is one element
safetail7 [x] = [x]
safetail7 (_ : xs) = xs

safetail8
  = \ xs ->
      case xs of
          [] -> []
          (_ : xs) -> xs

-------------------------------------------------------------------

-- Redefine the || operator
-- Enter this in your ghci first:
--
-- import Prelude hiding ((||))

-- False || False = False
-- _ || _ = True

-- False || b = b
-- True || _ = True

-- syntax is fine, but logic is wrong
-- b || c
--   | b == c = True
--   | otherwise = False

-- b || c
--   | b == c = b
--   | otherwise = True

-- b || False = b
-- _ || True = True

-- b || c
--   | b == c = c
--   | otherwise = True

-- Doesn't work
-- b || True = b
-- _ || True = True

-- False || False = False
-- False || True = True
-- True || False = True
-- True || True = True

-------------------------------------------------------------------

-- Redefine the && operator
-- Enter this in your ghci first:
--
-- import Prelude hiding ((&&))

-- True && True = True
-- _ && _ = False

-- a && b = if a then if b then True else False else False

-- incorrect logic
-- a && b = if not (a) then not (b) else True

-- doesn't work
-- a && b = if a then b

-- wrong logic
-- a && b = if a then if b then False else True else False

-- a && b = if a then b else False

-- a && b = if b then a else False


-------------------------------------------------------------------

-- What is another way of writing
-- mult x y z = x * y * z
-- ?

-- mult x y z = \ x -> (\ y -> (\ z -> x * y * z))

-- mult = \x -> (x * \y -> (y * \z -> z))

-- this one is right
-- mult = \x -> (\y -> (\z -> x * y * z))

-- mult = ((((\x -> \y) -> \z) -> x * y) * z)

-------------------------------------------------------------------

-- what does this expression mean?
-- f x g y

-- ((f x) g) y

-------------------------------------------------------------------

-- The type signature
-- f :: (a -> a) -> a
-- means that f

-- takes a function as its argument?  Yes
-- takes two arguments one at a time?  No
-- takes a pair of arguments? No
-- returns a function as its result? No
-- returns a pair of results? No

-------------------------------------------------------------------

-- Choose the correct implementation of remove :: Int -> [a] -> [a]
-- which removes the nth element from a list
--
-- remove 0 [1, 2, 3, 4] = [2, 3, 4]

remove n xs = take n xs ++ drop (n + 1) xs

-------------------------------------------------------------------

-- What is the output of the function call
-- funct 3 [1, 2, 3, 4, 5, 6, 7]
-- ?

funct :: Int -> [a] -> [a]
funct x xs = take (x + 1) xs ++ drop x xs

-- [1, 2, 3, 4, 4, 5, 6, 7]

-------------------------------------------------------------------



