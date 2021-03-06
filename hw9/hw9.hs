{-# LANGUAGE NPlusKPatterns #-}

import Data.List
import Data.Char
import Unsafe.Coerce

data Nat = Zero
         | Succ Nat
	 deriving Show

--
-- test with
--
-- natToInteger Zero
-- 0
-- natToInteger (Succ (Succ (Succ Zero)))
-- 3
--

natToInteger :: Nat -> Integer


-- works
natToInteger Zero = 0
natToInteger (Succ n) = natToInteger n + 1

{-
-- works
natToInteger (Succ n) = natToInteger n + 1
natToInteger Zero = 0
-}

-- infinite loop
-- natToInteger n = natToInteger n

{-
-- works
natToInteger (Succ n) = 1 + natToInteger n
natToInteger Zero = 0
-}

{-
-- always returns 1
natToInteger Zero = 1
natToInteger (Succ n) = (1 + natToInteger n) - 1
-}

{-
-- works
natToInteger = head . m
  where m Zero = [0]
  	m (Succ n) = [sum [x | x <- (1 : m n)]]
-}

{-
-- works
natToInteger :: Nat -> Integer
natToInteger = \ n -> genericLength [c | c <- show n, c == 'S']
-}

{-
-- doesn't compile: Integer != Int
natToInteger :: Nat -> Integer
natToInteger = \ n -> length [c | c <- show n, c == 'S']
-}

------------------------------------------------------------

integerToNat :: Integer -> Nat

-- works
integerToNat 0 = Zero
integerToNat (n+1) = Succ (integerToNat n)


{-
-- totally broken
integerToNat 0 = Succ Zero
integerToNat n = (Succ (integerToNat n))
-}


{-
-- Not sure what it doesn't, but it isn't close
integerToNat n = product [(unsafeCoerce c) :: Integer | c <- show n]
-}

{-
integerToNat n = integerToNat n
-}

{-
-- works
integerToNat (n+1) = Succ (integerToNat n)
integerToNat 0 = Zero
-}

{-
-- works
integerToNat (n+1) = let m = integerToNat n in Succ m
integerToNat 0 = Zero
-}

{-
-- doesn't do the conversion
integerToNat = head . m
  where {
  	; m 0 = [0]
	; m (n + 1) = [sum [x | x <- (1 : m n)]]
  	}
-}

{-
-- type error
integerToNat :: Integer -> Nat
integerToNat = \ n -> genericLength [c | c <- show n, isDigit c]
-}


------------------------------------------------------------

add :: Nat -> Nat -> Nat

-- natToInteger (add m n) = natToInteger m + natToInteger n
-- natToInteger (add (integerToNat 95) (integerToNat 33))
-- 128


{-
-- works
add Zero n = n
add (Succ m) n = Succ (add n m)
-}

{-
-- works
add (Succ m) n = Succ (add n m)
add Zero n = n
-}

{-
-- broken
add Zero n = Zero
add (Succ m) n = Succ (add m n)
-}

{-
-- broken
add (Succ m) n = Succ (add m n)
add Zero n = Zero
-}

{-
-- broken
add n Zero = Zero
add n (Succ m) = Succ (add n m)
-}

{-
-- broken
add n (Succ m) = Succ (add n m)
add n Zero = Zero
-}

-- works
add n Zero = n
add n (Succ m) = Succ (add m n)


{-
-- works
add n (Succ m) = Succ (add m n)
add n Zero = n
-}

------------------------------------------------------------

mult :: Nat -> Nat -> Nat

-- natToInteer (mult m n) = natToInteger m * natToInteger n

{-
mult Zero Zero = Zero
mult m (Succ n) = add m (mult m n)
-}


-- correct
mult m Zero = Zero
mult m (Succ n) = add m (mult m n)


{-
mult m Zero = Zero
mult m (Succ n) = add n (mult m n)
-}

{-
mult m Zero = Zero
mult m n = add m (mult m (Succ n))
-}


------------------------------------------------------------

class Monoid a where
        mempty :: a
	(<>) :: a -> a -> a

instance Monoid [a] where
	mempty = []
	(<>) = (++)

------------------------------------------------------------

{-
class Functor f where
        fmap :: (a -> b) -> f a -> f b

instance Main.Functor Maybe where
	fmap _ Nothing = Nothing
	fmap f (Just a) = Just (f a)
-}
------------------------------------------------------------


class (Functor f) => Foldable f where
        fold :: (Monoid m) => f m -> m


{-
instance Foldable [] where
	fold = foldl (map . (<>)) mempty
-}

{-
instance Foldable [] where
	fold xs = map (<>) xs
-}


-- only one that type checks
instance Foldable [] where
	fold = foldr (<>) mempty


{-
instance Foldable [] where
	fold xs = concat . map (mempty)
-}

------------------------------------------------------------
------------------------------------------------------------

