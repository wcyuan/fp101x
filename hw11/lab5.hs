module Lab5 where

import Control.Monad

data Concurrent a = Concurrent ((a -> Action) -> Action)

data Action 
    = Atom (IO Action)
    | Fork Action Action
    | Stop

instance Show Action where
    show (Atom x) = "atom"
    show (Fork x y) = "fork " ++ show x ++ " " ++ show y
    show Stop = "stop"

-- ===================================
-- Ex. 0
-- ===================================

{-
How do you actually use this Action type?  What are examples of Actions?

This works:
  > Stop
  stop

These don't:
  > Atom Stop
  > Atom (putStrLn "hi")
Atom takes something of type "IO Action", not just Action, and not just "IO ()"
What is type "IO Action"?  That's an IO Monad that wraps an Action.  How do you
create a monad wrapping an action?  Use "return":

  > Atom (return Stop)
  atom

But that doesn't do anthing, how do you get an atom that does
something?  Use "do".  These are equivalent:

  > Atom (do () <- putStrLn "hi"; return Stop)
  atom
  > Atom (putStrLn "hi" >> return Stop)
  atom
  > Atom $ putStrLn "hi" >> return Stop
  atom

Fork is easier, you can just do:

  > Fork Stop Stop
  fork stop stop
  > Fork Stop (Atom (do () <- putStrLn "hi"; return Stop))
  fork stop atom

So how do you say "do x then do y"?  I guess it's this:

  > Atom (do () <- putStrLn "first"; return (Atom (do () <- putStrLn "second"; return Stop)))

Something like this might be cleaner:
  > let second = Atom (do () <- putStrLn "second"; return Stop)
  > let first = Atom (do () <- putStrLn "first"; return second)
Sort of funny that we have to write them backwards.
-}

{-

So what is Concurrent?  It is something that takes a continuation and returns an action.

What is a continuation?  Is it something that takes a value and
returns an action.  So a simple continuation is:

  > let foo = \a -> stop
  foo

So a Concurrent should take foo and return an action.  Here is a simple concurrent.

  > let c = Concurrent (\f -> Stop)

So, the function in Concurrent can take a foo, but you have to extract
that function out to use it.  Here's a function that extracts the data
out of a Concurrent

  > let app (Concurrent c) = c
  > app c foo
  stop

I think this tells me how to do the first exercise.  We have to take
the Concurrent argument, extract out the function, and apply that
function on a continuation that we create.  The continuation we create
should just be a simple continuation using Stop:

-}

action :: Concurrent a -> Action
-- action = error "You have to implement action"
action (Concurrent c) = c (\_ -> Stop)

{-
Ex0
 > action (Concurrent (\a -> Stop))
 stop
Ex1
 > :type action (Concurrent (\a -> Stop))
 action (Concurrent (\a -> Stop)) :: Action
Ex2
 > action (Concurrent (\a -> Fork Stop $ Fork Stop Stop))
 fork stop fork stop stop
Ex3
 > action (Concurrent (\a -> Atom $ putStr "Haskell"))

 <interactive>:96:34:
    Couldn't match type `()' with `Action'
    Expected type: IO Action
      Actual type: IO ()
    In the return type of a call of `putStr'
    In the second argument of `($)', namely `putStr "Haskell"'
    In the expression: Atom $ putStr "Haskell"
Ex4
 > action (Concurrent (\a -> Atom $ do () <- putStr "Haskell"; return Stop))
 atom
Ex5
 > :type Concurrent (\a -> Atom $ putStr "Haskell" >> return Stop)
 Concurrent (\a -> Atom $ putStr "Haskell" >> return Stop)
  :: Concurrent a
-}


-- ===================================
-- Ex. 1
-- ===================================

stop :: Concurrent a
-- stop = error "You have to implement stop"
stop = Concurrent (\_ -> Stop)

{-
Ex6
 > action stop
 stop
Ex7
 > stop

 <interactive>:104:1:
    No instance for (Show (Concurrent a0))
      arising from a use of `print'
    Possible fix:
      add an instance declaration for (Show (Concurrent a0))
    In a stmt of an interactive GHCi command: print it

-}

-- ===================================
-- Ex. 2
-- ===================================

atom :: IO a -> Concurrent a
atom = error "You have to implement atom"


-- ===================================
-- Ex. 3
-- ===================================

fork :: Concurrent a -> Concurrent ()
fork = error "You have to implement fork"

par :: Concurrent a -> Concurrent a -> Concurrent a
par = error "You have to implement par"


-- ===================================
-- Ex. 4
-- ===================================

instance Monad Concurrent where
    (Concurrent f) >>= g = error "You have to implement >>="
    return x = Concurrent (\c -> c x)


-- ===================================
-- Ex. 5
-- ===================================

roundRobin :: [Action] -> IO ()
roundRobin = error "You have to implement roundRobin"

-- ===================================
-- Tests
-- ===================================

ex0 :: Concurrent ()
ex0 = par (loop (genRandom 1337)) (loop (genRandom 2600) >> atom (putStrLn ""))

ex1 :: Concurrent ()
ex1 = do atom (putStr "Haskell")
         fork (loop $ genRandom 7331) 
         loop $ genRandom 42
         atom (putStrLn "")


-- ===================================
-- Helper Functions
-- ===================================

run :: Concurrent a -> IO ()
run x = roundRobin [action x]

genRandom :: Int -> [Int]
genRandom 1337 = [1, 96, 36, 11, 42, 47, 9, 1, 62, 73]
genRandom 7331 = [17, 73, 92, 36, 22, 72, 19, 35, 6, 74]
genRandom 2600 = [83, 98, 35, 84, 44, 61, 54, 35, 83, 9]
genRandom 42   = [71, 71, 17, 14, 16, 91, 18, 71, 58, 75]

loop :: [Int] -> Concurrent ()
loop xs = mapM_ (atom . putStr . show) xs
