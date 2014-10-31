-------------------------------------------------
e0 = [False, True, False, True]
-- type is [Bool]

-------------------------------------------------
-- possible types include
-- [Integer] -- no
-- [[Integer]] -- yes
-- [[Int]] -- yes
-- [(Integer, Integer)] -- no
-- [[Int, Int]] -- is this a valid type? No
-- [[Integer, Integer]] -- yes if the one above is, but it isn't
-- [[a]] -- no
-- Num t => [[t]] -- yes

e1 :: Num t => [[t]]
e1 = [[1,2], [3,4]]

-- This gives an error:
--
-- foo :: [[Int, Int]] -> Int
-- foo x = x !! 0 !! 0


-------------------------------------------------
-- this gives an error
-- e2 :: Eq t => [[[t]]]
e2 = [[[1, 2, 3]], [[3, 4, 5]]]


-------------------------------------------------
e3 :: Num a => a -> a
e3 x = x * 2

e4 :: (t, t1) -> t
e4 (x, y) = x

e5 :: (t, t1, t2) -> t2
e5 (x, y, z) = z

e6 :: Num a => a -> a -> a
e6 x y = x * y

e7 :: (t1, t) -> (t, t1)
e7 (x, y) = (y, x)

e8 :: t1 -> t -> (t, t1)
e8 x y = (y, x)

e9 :: [t] -> (t, Bool)
e9 [x, y] = (x, True)

e10 :: (t, t) -> [t]
e10 (x, y) = [x, y]

e11 :: (Char, Bool)
e11 = ('\a', False)

e12 :: [(Char, Int)]
e12 = [('a', 1)]

e13 :: Int -> Int -> Int
e13 x y = x + y * y

e14 :: ([Char], [Float])
e14 = ("Haskell", [3.1, 3.14, 3.141, 3.1415])

e15 :: [a] -> [b] -> (a, b)
e15 xs ys = (head xs, head ys)
