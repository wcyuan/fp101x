------------------------------------------------------------------------------------------------------------------------------
-- ROSE TREES, FUNCTORS, MONOIDS, FOLDABLES
------------------------------------------------------------------------------------------------------------------------------

data Rose a = a :> [Rose a] deriving Show

-- ===================================
-- Ex. 0-2
-- ===================================

root :: Rose a -> a 
-- root = error "you have to implement root" 
root (r :> c) = r

children :: Rose a -> [Rose a]
-- children = error "you have to implement children"
children (r :> c) = c

xs = 0 :> [1 :> [2 :> [3 :> [4 :> [], 5 :> []]]], 6 :> [], 7 :> [8 :> [9 :> [10 :> []], 11 :> []], 12 :> [13 :> []]]]

ex2 = root . head . children . head . children . head . drop 2 $ children xs

{-
*Main> root (1 :> [2 :> [], 3 :> []])
1
*Main> root ('a' :> [])
'a'
*Main> children (1 :> [2 :> [], 3 :> []])
[2 :> [],3 :> []]
*Main> children ('a' :> [])
[]
*Main> let tree = 'x' :> map (flip (:>) []) ['a'..'x']
*Main> length $ children tree
24
*Main> let tree = 'x' :> map (\c -> c :> []) ['a'..'A']
*Main> length (children tree)
0
*Main> let xs = 0 :> [1 :> [2 :> [3 :> [4 :> [], 5 :> []]]], 6 :> [], 7 :> [8 :> [9 :> [10 :> []], 11 :> []], 12 :> [13 :> []]]]
*Main> root . head . children . head . children . head . drop 2 $ children xs
9
*Main> ex2
9
-}

-- ===================================
-- Ex. 3-7
-- ===================================

size :: Rose a -> Int
-- size = error "you have to implement size"
size (a :> c) = 1 + sum (map size c)

{-
*Main> size xs
14
*Main> size (1 :> [])
1
*Main> size (1 :> [2:> []])
2
*Main> size (1 :> [2:> [], 3:>[]])
3
*Main> size (1 :> [2 :> [4 :> []], 3 :> []])
4
*Main> let tree = 1 :> map (\c -> c :> []) [1..5]
*Main> size tree
6
*Main> let tree = 1 :> map (\c -> c :> []) [1..5]
*Main> size . head . children $ tree
1
-}

leaves :: Rose a -> Int
-- leaves = error "you have to implement leaves"
leaves (a :> []) = 1
leaves (a :> (c:cs)) = sum (map leaves (c:cs))

ex7 = (*) (leaves . head . children . head . children $ xs) (product . map size . children . head . drop 2 . children $ xs)

{-
*Main> leaves xs
6
*Main> leaves (1 :> [2 :> [4 :> []], 3 :> []])
2
*Main> leaves (1 :> [2 :> [], 3 :> []])
2
*Main> leaves (1 :> [3 :> []])
1
*Main> leaves (1 :> [])
1
*Main> let tree = 1 :> map (\c -> c :> []) [1..5]
*Main> leaves tree
5
*Main> let tree = 1 :> map (\c -> c :> []) [1..5]
*Main> product (map leaves (children tree))
1
*Main> xs
0 :> [1 :> [2 :> [3 :> [4 :> [],5 :> []]]],6 :> [],7 :> [8 :> [9 :> [10 :> []],11 :> []],12 :> [13 :> []]]]
*Main> (*) (leaves . head . children . head . children $ xs) (product . map size . children . head . drop 2 . children $ xs)
16
-}

-- ===================================
-- Ex. 8-10
-- ===================================

instance Functor Rose where
  fmap = error "you have to implement fmap for Rose"

ex10 = round . root . head . children . fmap (\x -> if x > 0.5 then x else 0) $ fmap (\x -> sin(fromIntegral x)) xs

-- ===================================
-- Ex. 11-13
-- ===================================

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m

newtype Sum a = Sum a
newtype Product a = Product a

instance Num a => Monoid (Sum a) where
  mempty = error "you have to implement mempty for Sum"
  mappend = error "you have to implement mappend for Sum"
  
instance Num a => Monoid (Product a) where
  mempty = error "you have to implement mempty for Product"
  mappend = error "you have to implement mappend for Product"

unSum :: Sum a -> a
unSum = error "you have to implement unSum"
unProduct :: Product a -> a
unProduct = error "you have to implement unProduct"

num1 = mappend (mappend (Sum 2) (mappend (mappend mempty (Sum 1)) mempty)) (mappend (Sum 2) (Sum 1))
  
num2 = mappend (Sum 3) (mappend mempty (mappend (mappend (mappend (Sum 2) mempty) (Sum (-1))) (Sum 3)))
  
ex13 = unSum (mappend (Sum 5) (Sum (unProduct (mappend (Product (unSum num2)) (mappend (Product (unSum num1)) (mappend mempty (mappend (Product 2) (Product 3))))))))

-- ===================================
-- Ex. 14-15
-- ===================================

class Functor f => Foldable f where
  fold :: Monoid m => f m -> m
  foldMap :: Monoid m => (a -> m) -> (f a -> m)
  foldMap = error "you have to implement foldMap"
  
instance Foldable Rose where
  fold = error "you have to implement fold for Rose"
  
sumxs = Sum 0 :> [Sum 13 :> [Sum 26 :> [Sum (-31) :> [Sum (-45) :> [], Sum 23 :> []]]], Sum 27 :> [], Sum 9 :> [Sum 15 :> [Sum 3 :> [Sum (-113) :> []], Sum 1 :> []], Sum 71 :> [Sum 55 :> []]]]

ex15 = unSum (mappend (mappend (fold sumxs) (mappend (fold . head . drop 2 . children $ sumxs) (Sum 30))) (fold . head . children $ sumxs))

-- ===================================
-- Ex. 16-18
-- ===================================

ex17 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (mappend (foldMap (\x -> Sum x) . head . drop 2 . children $ xs) (Sum 30))) (foldMap (\x -> Sum x) . head . children $ xs))

ex18 = unSum (mappend (mappend (foldMap (\x -> Sum x) xs) (Sum (unProduct (mappend (foldMap (\x -> Product x) . head . drop 2 . children $ xs) (Product 3))))) (foldMap (\x -> Sum x) . head . children $ xs))

-- ===================================
-- Ex. 19-21
-- ===================================

fproduct, fsum :: (Foldable f, Num a) => f a -> a
fsum = error "you have to implement fsum"
fproduct = error "you have to implement fproduct"

ex21 = ((fsum . head . drop 1 . children $ xs) + (fproduct . head . children . head . children . head . drop 2 . children $ xs)) - (fsum . head . children . head . children $ xs)

