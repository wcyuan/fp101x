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

  > let foo = \a -> Stop
  foo
  > let foo = \a -> Atom $ putStrLn a >> return Stop
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
-- atom :: IO a -> ((a -> Action) -> Action)
-- atom = error "You have to implement atom"

atom io_a = Concurrent $ \ f -> Atom $ io_a >>= (\ a -> return (f a))

{-
Ex8
 > action . atom . putStrLn $ "Haskell"
 atom
Ex9
 > action $ atom undefined
 atom
Ex10
 > atom . putStrLn $ "Haskell"

 <interactive>:148:1:
    No instance for (Show (Concurrent ()))
      arising from a use of `print'
    Possible fix:
      add an instance declaration for (Show (Concurrent ()))
    In a stmt of an interactive GHCi command: print it
-}

-- ===================================
-- Ex. 3
-- ===================================

fork :: Concurrent a -> Concurrent ()
--fork = error "You have to implement fork"
--
-- This type checks, but it doesn't seem right.  I need to use () somewhere
-- fork c = Concurrent $ \ f -> action c
--
-- This also type checks, but also doesn't seem right, it's not using c anywhere
-- fork (Concurrent c) = Concurrent $ \f -> f ()
--
-- also, for both of those
--  > action $ fork stop
--  stop
-- which isn't one of the choices.  All the choices involve a fork, so
-- it looks like it needs to create a fork somewhere, which makes
-- sense given the name of the function, but the instructions don't
-- say anything about creating a fork

-- This seems so promising, but it doesn't type check
-- fork c = Concurrent $ \ f -> f (\() -> (action c))
--
-- doesn't type check:
-- fork (Concurrent c) = Concurrent $ \ f -> Fork (c ()) (f ())

fork c = Concurrent $ \ f -> Fork (action c) (f ())

{-
Ex11
 > action $ fork stop
 fork stop stop
Ex12
 > action (fork (atom (putStr "Hacker")))
 fork atom stop
Ex13
 > :type action (fork (atom (putStr "Hacker")))
 action (fork (atom (putStr "Hacker"))) :: Action
Ex14
 > action (fork undefined)
 fork *** Exception: Prelude.undefined
-}

par :: Concurrent a -> Concurrent a -> Concurrent a
-- par = error "You have to implement par"
-- par c1 c2 = Concurrent $ \f -> fork c1 fork c2
par (Concurrent a) (Concurrent b) = Concurrent $ \f -> Fork (a f) (b f)

{-
Ex15
 > action $ par stop stop
 fork stop stop
Ex16
 > action (par (atom (putStr "think")) (atom (putStr "hack")))
 fork atom atom
Ex17
 > action (par stop $ fork stop)
 fork stop fork stop stop
Ex18
 > action $ par (atom $ putChar 'x') (fork stop)
 fork atom fork stop stop
-}

-- ===================================
-- Ex. 4
-- ===================================

-- http://en.wikibooks.org/wiki/Haskell/Continuation_passing_style

-- you probably need a "let ... in ... " expression to unwrap the Concurrent

instance Monad Concurrent where
    -- (>>=) :: (Concurrent a) -> (a -> (Concurrent b)) -> (Concurrent b)
    -- (Concurrent f) >>= g = error "You have to implement >>="

    -- This type checks, but is obviously wrong
    -- (Concurrent f) >>= g = Concurrent $ \ h -> Stop
    -- does not type check

    -- f :: (a -> Action) -> Action
    -- g :: (a -> (Concurrent b))
    -- k :: (b -> Action)
    -- x :: a
    (Concurrent f) >>= g = Concurrent $ \ k -> f $ \ x -> let
      (Concurrent b) = g x
      in b k

    return x = Concurrent (\c -> c x)


-- ibind :: ((a -> Int) -> Int) -> (a -> ((b -> Int) -> Int)) -> ((b -> Int) -> Int)
-- ibind f g = \a -> g (f a)

bind :: ((a -> Action) -> Action) -> (a -> ((b -> Action) -> Action)) -> ((b -> Action) -> Action)
bind s f = \ k -> s $ \x -> f x $ k

-- k :: b -> Action
-- s :: (a -> Action) -> Action
-- f :: a -> ((b -> Action) -> Action)
-- x :: a

{-
 An aborted attempt:
bind f g = \ bfunc -> (\ a c -> let
     s = f c
     t = c a
     u = g a
     in u bfunc) 1 (\x -> Stop)
-}


-- Another aborted attempt:
--ibind :: ((a -> Int) -> Int)
-- this type checks but is useless
-- ibind f = f undefined
--ibind f = (\a -> f a) undefined

{-
Ex19
 > action (stop >>= (\c -> stop))
 stop
Ex20
 > action (atom (putStrLn "whatever...") >>= stop)
 <interactive>:129:43:
    Couldn't match expected type `() -> Concurrent a0'
                with actual type `Concurrent a1'
    In the second argument of `(>>=)', namely `stop'
    In the first argument of `action', namely
      `(atom (putStrLn "whatever...") >>= stop)'
    In the expression: action (atom (putStrLn "whatever...") >>= stop)
Ex21
 > stop >>= stop

 <interactive>:130:10:
    Couldn't match expected type `a0 -> Concurrent b0'
                with actual type `Concurrent a1'
    In the second argument of `(>>=)', namely `stop'
    In the expression: stop >>= stop
    In an equation for `it': it = stop >>= stop
Ex22
 > :t stop >>= stop

 <interactive>:1:10:
    Couldn't match expected type `a0 -> Concurrent b0'
                with actual type `Concurrent a1'
    In the second argument of `(>>=)', namely `stop'
    In the expression: stop >>= stop
Ex23
 > action (fork stop >>= \_ -> fork stop)
 fork stop fork stop stop
-}

-- ===================================
-- Ex. 5
-- ===================================

roundRobin :: [Action] -> IO ()
--roundRobin = error "You must implement roundRobin"
roundRobin [] = return ()
roundRobin (Stop:as) = roundRobin as
roundRobin (Fork a b:as) = roundRobin (as ++ [a] ++ [b])
-- these type check but are obviously wrong
--
-- this goes into an infinite loop
-- roundRobin (Atom io_act:as) = roundRobin (as ++ [Atom $ io_act >>= \act -> return act])
--
-- this just stops:
-- roundRobin (Atom io_act:as) = roundRobin (as ++ [Stop])

roundRobin (Atom io_act:as) = io_act >>= \act -> roundRobin (as ++ [act])

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

{-
Ex24
 > run ex0
 183969836351184424447619541356283739
Ex25
 > run ex1
 Haskell177173719217361422167291191835716587475
-}

-- ===================================
-- examples from the forums
-- ===================================

myex1 = run $ fork (ho >> ho >> ho) >>
                   (hi >> hi >> hi) >> atom (putStr "\n")
  where ho = atom (putStr "ho")
        hi = atom (putStr "hi")

-- Should produce: hohihohihohi
-- If we fork twice, something interesting happens:

myex2 = run $ fork (put3 "ba") >> fork (put3 "di") >>
        put3 "bu" >> atom (putStr "\n")
  where put3 = sequence . take 3 . repeat . atom . putStr

-- In my implementation, this produces: babadibubadibudibu
-- Finally, an example using par:

myex3 = run $ par (put3 "ba") (put3 "di" >> stop) >>
              atom (putStr "\n")
  where put3 = sequence . take 3 . repeat . atom . putStr

-- 

syracuse :: Int -> Concurrent Int
syracuse 1 = return 1
syracuse n = (print n) >>= syr >>= syracuse
  where print n = do atom $ putStr $ (show  n) ++ " " 
                     return n
        syr n | n `mod` 2 == 0 = return (n `div` 2)
              | otherwise      = return (3 * n + 1) 

ex6 :: Int -> Concurrent ()
ex6 n = do syracuse n
           atom $ putStrLn ""

-- Testing it:
-- Lab5> run $ ex6 15
-- 15 46 23 70 35 106 53 160 80 40 20 10 5 16 8 4 2


