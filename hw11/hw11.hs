fibs :: [Integer]

-- never returns
-- fibs = 1 : [x + y | (x, y) <- zip fibs (tail fibs)]

-- after 1, the rest are zero
-- fibs = 0 : 1 : zipWith (*) fibs (tail fibs)


fibs = 0 : 1 : [x + y | (x, y) <- zip fibs (tail fibs)]

-- works, but starts on 1 instead of 0
-- fibs = 1 : 1 : [x + y | (x, y) <- zip (tail fibs) fibs]

----------------------------------------------------------------------

-- fib should return the nth element of the fibo series
-- zero indexed
fib :: Int -> Integer

-- These first two work, but they consider zero the first element of the
-- fibo series, not the zeroth element

-- fib n = last (take n fibs)
-- fib n = head (drop (n - 1) fibs)

fib n = fibs !! n

-- index is undefined
-- fib n = index fibs n

----------------------------------------------------------------------

{-
EXERCISE 8

Choose the correct definition for the expression 
largeFib :: Integer
that uses fibs from the previous exercises to calculate the first
Fibonacci number greater than 1000.
-}

largeFib :: Integer

largeFib = head (dropWhile (<= 1000) fibs)

-- largeFib = last (take 19 fibs)

-- largeFib = filter (> 1000) fibs

-- largeFib = head (drop 1000 fibs)

----------------------------------------------------------------------

{-
EXERCISE 9 (1 point possible)

Choose the correct definition for the
function
-}
repeatTree :: a -> Tree a
{-
for the following type of binary
trees:
-}

data Tree a = Leaf
     	    | Node (Tree a) a (Tree a)

{-
The behavior should be analogous to that of the library function
repeat (not considering bottom and partial cases):

repeat :: a -> [a]
repeat x = xs
  where xs = x : xs
-}

-- repeatTree x = Node x x x

repeatTree x = Node t x t
  where t = repeatTree x

-- repeatTree x = repeatTree (Node Leaf x Leaf)

{-
repeatTree x = Node t x t
  where t = repeatTree (Node t x t)
-}

