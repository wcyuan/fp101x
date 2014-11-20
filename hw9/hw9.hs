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


{-
-- works
natToInteger Zero = 0
natToInteger (Succ n) = natToInteger n + 1
-}

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

