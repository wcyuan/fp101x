import Data.Char

----------------------------------------------------------
sum100 = sum[x ^ 2 | x <- [1..100]]
----------------------------------------------------------
replicate n a = [a | _ <- [1 .. n]]

----------------------------------------------------------

-- this is even better, it recognizes that order doesn't matter
-- pyths n = [(x, y, z) | x <- [1 .. n], y <- [x .. n], z <- [y .. n],
--            x ^ 2 + y ^ 2 == z ^ 2]

pyths n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n],
           x ^ 2 + y ^ 2 == z ^ 2]

----------------------------------------------------------

factors n = [x | x <- [1..n], n `mod` x == 0]
perfects n = [x | x <- [1..n], isPerfect x]
  where isPerfect num = sum(init(factors num)) == num

----------------------------------------------------------
q5a = [(x, y) | x <- [1, 2, 3], y <- [4, 5, 6]]

-- this is equivalent to

q5b = concat [[(x, y) | y <- [4, 5, 6]] | x <- [1, 2, 3]]

----------------------------------------------------------

-- given a key and a key-map, returns the value for the given key
find :: (Eq a) => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

-- given a value and a list, returns the index of that value in the list
positions1 x xs = [i | (x', i) <- zip xs [0..n], x == x']
  where n = length xs - 1

positions :: (Eq a) => a -> [a] -> [Int]
positions x xs = find x (zip xs [0 ..n])
  where n = length xs - 1

----------------------------------------------------------

-- sum ( (xs !! i) * (ys !! i) ) for i = 0 to n-1

scalarproduct xs ys = sum [x * y | (x, y) <- xs `zip` ys]

----------------------------------------------------------

-- original:

-- let2int :: Char -> Int
-- let2int c = ord c - ord 'a'

-- int2let :: Int -> Char
-- int2let n = chr (ord 'a' + n)

-- shift :: Int -> Char -> Char
-- shift n c =
--   | isLower c = int2let ((let2int c + n) `mod` 26)
--   | otherwise = c

-- encode :: Int -> String -> String
-- encode n xs = [shift n x | x <- xs]

-- Changed, in a hacky way, to handle upper case:

let2int :: Char -> Int
let2int c
  | isLower c = ord c - ord 'a'
  | isUpper c = ord c - ord 'A'
  | otherwise = ord c

int2let :: Int -> Char -> Char
int2let n c
  | isLower c = chr (ord 'a' + n)
  | isUpper c = chr (ord 'A' + n)
  | otherwise = c

shift :: Int -> Char -> Char
shift n c = int2let ((let2int c + n) `mod` 26) c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

----------------------------------------------------------
----------------------------------------------------------
----------------------------------------------------------
----------------------------------------------------------
----------------------------------------------------------
----------------------------------------------------------
----------------------------------------------------------
----------------------------------------------------------
