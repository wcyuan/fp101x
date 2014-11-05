-- evens [2, 5, 6, 13, 32] = [2, 6, 32]
evens xs = [x | x <- xs, even x]

-- squares 4 = [1*1, 2*2, 3*3, 4*4]
-- squares 0 = []
squares :: Integer -> [Integer]
squares n = [x*x | x <- [1..n]]

sumSquares :: Integer -> Integer
sumSquares n = sum (squares n)

-- squares' 4 2 = [3*3, 4*4, 5*5, 6*6]
-- squares' 2 0 = [1*1, 2*2]
-- squares' 0 2 = []
-- squares' 0 0 = []
squares' m n = [x*x | x <- [(n+1)..(m+n)]]

sumSquares' x = sum . uncurry squares' $ (x, x)

-- coords 1 1 = [(0,0), (0,1), (1,0), (1,1)]
-- coords 1 2 = [(0,0), (0,1), (0,2), (1,0), (1, 1), (1, 2)]
coords :: Integer -> Integer -> [(Integer, Integer)]
coords m n = [(x, y) | x <- [0..m], y <- [0..n]]

