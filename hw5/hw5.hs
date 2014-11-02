------------------------------------------------------------------
import Prelude hiding ((^) (and))

-- exponentiation
--m ^x 0 = 1
--m ^x n = m * m ^x (n - 1)

-- True iff all elements in a list are True
andx [] = True
andx (b : bs)
  | b = andx bs
  | otherwise = False

-- concatenate two lists
concatx [] = []
concatx (xs : xss) = xs ++ concat xss

-- return a list of a single value repeated n times
replicatex 0 _ = []
replicatex n x = x : replicate (n - 1) x

-- selectx
-- return the nth element of a list
--selectx :: [a] -> Int -> a
selectx (x : _) 0 = x
selectx (_ : xs) n = selectx xs (n-1)

-- elemx : true if a is in the list
elemx :: Eq a => a -> [a] -> Bool
elemx _ [] = False
elemx x (y : ys)
  | x == y = True
  | otherwise = elemx x ys

-- merge sorted lists
-- mergex [2, 5, 6] [1, 3, 4]
-- [1, 2, 3, 4, 5, 6]
mergex :: Ord a => [a] -> [a] -> [a]
mergex [] ys = ys
mergex xs [] = xs
mergex (x : xs) (y : ys)
  = if x <= y then x : mergex xs (y : ys) else y : mergex (x : xs) ys

-- merge sort
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = mergex (msort zs) (msort ys)
  where (ys, zs) = halve xs

------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
------------------------------------------------------------------
