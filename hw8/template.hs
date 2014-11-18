module Lab4 where

------------------------------------------------------------------------------------------------------------------------------
-- RECURSIVE FUNCTIONS
------------------------------------------------------------------------------------------------------------------------------

import Data.Char

-- ===================================
-- Ex. 0
-- ===================================

triangle :: Integer -> Integer
triangle 0 = 0
triangle n = n + triangle (n-1)

-- ===================================
-- Ex. 1
-- ===================================

count :: Eq a => a -> [a] -> Int
count a [] = 0
count a (x:xs) = (if x == a then 1 else 0) + count a xs

xs = [1,2,35,2,3,4,8,2,9,0,5,2,8,4,9,1,9,7,3,9,2,0,5,2,7,6,92,8,3,6,1,9,2,4,8,7,1,2,8,0,4,5,2,3,6,2,3,9,8,4,7,1,4,0,1,8,4,1,2,4,56,7,2,98,3,5,28,4,0,12,4,6,8,1,9,4,8,62,3,71,0,3,8,10,2,4,7,12,9,0,3,47,1,0,23,4,8,1,20,5,7,29,3,5,68,23,5,6,3,4,98,1,0,2,3,8,1]
ys = map (\x -> ((x + 1) * 3) ^ 3 - 7) xs

poem = [ "Three Types for the Lisp-kings under the parentheses,"
       , "Seven for the Web-lords in their halls of XML,"
       , "Nine for C Developers doomed to segfault,"
       , "One for the Dark Lord on his dark throne"
       , "In the Land of Haskell where the Monads lie."
       , "One Type to rule them all, One Type to find them,"
       , "One Type to bring them all and in the Lambda >>= them"
       , "In the Land of Haskell where the Monads lie."
       ]

-- ===================================
-- Ex. 2
-- ===================================

euclid :: (Int,  Int) -> Int
euclid (x, y) = if (x == y) then x else if (x < y) then (euclid (x, y-x)) else (euclid (x-y, y))

-- ===================================
-- Ex. 3
-- ===================================

funkyMap :: (a -> b) -> (a -> b) -> [a] -> [b]
funkyMapE f g [] = []
funkyMapE f g (x:xs) = (g x) : funkyMap f g xs
funkyMap f g [] = []
funkyMap f g (x:xs) = (f x) : funkyMapE f g xs

{-
*Lab4> :type \ a -> a
\ a -> a :: t -> t

*Lab4> :type [undefined]
[undefined] :: [t]
*Lab4> :type (True, (False))
(True, (False)) :: (Bool, Bool)
*Lab4> :type f a = \b -> (b, a)

<interactive>:1:5: parse error on input ‘=’
*Lab4> let f a = \b -> (b, a)
*Lab4> :type f
f :: t1 -> t -> (t, t1)
*Lab4> :type foldr id
foldr id :: b -> [b -> b] -> b
*Lab4> :type (a -> ([(a -> a)] -> a))

<interactive>:1:2:
    Pattern syntax in expression context: a -> ([(a -> a)] -> a)
*Lab4> :type flip foldr const
flip foldr const
  :: (a -> (a1 -> b -> a1) -> a1 -> b -> a1) -> [a] -> a1 -> b -> a1
*Lab4> let dup a = (a, a)
*Lab4> :type dup
dup :: t -> (t, t)
*Lab4> :type dup . dup . dup
dup . dup . dup :: a -> (((a, a), (a, a)), ((a, a), (a, a)))
*Lab4> let h g f = (f . g) $ f
*Lab4> :type h
h :: ((b -> c) -> b) -> (b -> c) -> c
*Lab4> let fix = h fix
*Lab4> :type fix
fix :: (b -> b) -> b
*Lab4> let f = \f n -> if (n == 0) then 1 else n * f (n - 1)
*Lab4> :type f
f :: (Num a, Eq a) => (a -> a) -> a -> a
*Lab4> f 10

<interactive>:44:1:
    No instance for (Show (a0 -> a0)) arising from a use of ‘print’
    In a stmt of an interactive GHCi command: print it
*Lab4> f 1

<interactive>:45:1:
    No instance for (Show (a0 -> a0)) arising from a use of ‘print’
    In a stmt of an interactive GHCi command: print it

*Lab4> let k = fix $ f
*Lab4> k 42
1405006117752879898543142606244511569936384000000000
*Lab4> :type k
k :: (Num a, Eq a) => a -> a
*Lab4> k 0
1
*Lab4> k 1
1
*Lab4> k 2
2
*Lab4> k 3
6
*Lab4> k 4
24
*Lab4> k 5
120
*Lab4> k 10
3628800
*Lab4> 

-}

