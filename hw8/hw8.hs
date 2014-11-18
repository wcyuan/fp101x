--
-- test with
-- putStr' "asdf"
-- putStr' ""
--

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) = putChar x >> putStr' xs

--------------------------------------------

-- test with
-- putStrLn' ""
-- putStrLn' "asdf"

putStrLn' :: String -> IO ()

-- works
putStrLn' [] = putChar '\n'
putStrLn' xs = putStr' xs >> putStrLn' ""

-- works
-- putStrLn' [] = putChar '\n'
-- putStrLn' xs = putStr' xs >> putChar '\n'

-- works
-- putStrLn' [] = putChar '\n'
-- putStrLn' xs = putStr' xs >>= \ x -> putChar '\n'

-- doesn't compile, returns a value
-- putStrLn' [] = putChar '\n'
-- putStrLn' xs = putStr' xs >> \ x -> putChar '\n'

-- works
-- putStrLn' [] = putChar '\n'
-- putStrLn' xs = putStr' xs >> putStr' "\n"

-- Infinite loop on any non-empty-string input
-- putStrLn' [] = putChar '\n'
-- putStrLn' xs = putStr' xs >> putStrLn' "\n"

-- doesn't compile, bad return type
-- putStrLn' [] = return ""
-- putStrLn' xs = putStrLn' xs >> putStr' "\n"

-- doesn't compile, can't putChar the String "\n"
-- putStrLn' [] = putChar "\n"
-- putStrLn' xs = putStr' xs >> putChar '\n'

--------------------------------------------

-- test with
-- getLine'
-- asdf
-- getLine'
-- asdf asdf
-- getLine'
-- 


getLine' :: IO String

get :: String -> IO String

-- stops after the first space
{-
getLine' = get ""
get xs
  = do x <- getChar
       case x of
           ' ' -> return xs
    	   '\n' -> return xs
	   _ -> get (xs ++ [x])
-}

{-
-- backwards
getLine' = get ""
get xs
  = do x <- getChar
       case x of
           '\n' -> return xs
           _ -> get (x : xs)
-}

-- works
getLine' = get []
get xs
  = do x <- getChar
       case x of
           '\n' -> return xs
           _ -> get (xs ++ [x])


{-
-- puts the newline at the beginning
getLine' = get []
get xs
  = do x <- getChar
       case x of
           '\n' -> return (x : xs)
           _ -> get (xs ++ [x])
-}

--------------------------------------------
interact' :: (String -> String) -> IO ()

interact' f
  = do input <- getLine'
       putStrLn' (f input)

{-
interact' f
  = do input <- getLine'
       putStrLn' input

interact' f
  = do input <- getChar
       putStrLn' (f input)

interact' f
  = do input <- getLine'
       putStr' (f input)
-}

--------------------------------------------

sequence_' :: Monad m => [m a] -> m ()

-- takes a finite, non-partial, list of non-bottom, monadic values,
-- and evaluates them in sequence, from left to right, ignoring all
-- (intermediate) results

--
-- to test, run:
--
-- :load hw8.hs ../hw7/parsing.lhs
-- Parsing.parse (sequence_' [Parsing.item, Parsing.item]) "asdfasdf"
-- [((), "dfasdf")]
-- Parsing.parse (sequence_' [Parsing.item, Parsing.item]) ""
-- []
-- Parsing.parse (sequence_' []) "asdf asdf"
-- [((),"asdf asdf")]
--


{-
-- doesn't compile, returns the wrong thing [] instead of ()
sequence_' [] = return []
sequence_' (m : ms) = m >> \ _ -> sequence_' ms
-}

{-
-- seems to work
sequence_' [] = return ()
sequence_' (m : ms) = (foldl (>>) m ms) >> return ()
-}

{-
-- doesn't compile
sequence_' ms = foldl (>>) (return ()) ms
-}

{-
-- seems to work
sequence_' [] = return ()
sequence_' (m : ms) = m >> sequence_' ms
-}

{-
-- seems to work
sequence_' [] = return ()
sequence_' (m : ms) = m >>= \ _ -> sequence_' ms
-}

-- doesn't compile
-- sequence_' ms = foldr (>>=) (return ()) ms

-- seems to work
sequence_' ms = foldr (>>) (return ()) ms

-- doesn't compile
-- sequence_' ms = foldr (>>) (return []) ms

--------------------------------------------
sequence' :: Monad m => [m a] -> m [a]

-- takes a finite, non-partial, list of non-bottom, monadic values,
-- and evaluates them in sequence, from left to right, collecting all
-- (intermediate) results into a list


--
-- to test, run:
--
-- :load hw8.hs ../hw7/parsing.lhs
-- Parsing.parse (sequence' [Parsing.item, Parsing.item]) "asdfasdf"
-- [("as", "dfasdf")]
-- Parsing.parse (sequence' [Parsing.item, Parsing.item]) "asdfasdf"
-- [("as", "dfasdf")]
-- Parsing.parse (sequence' [Parsing.item, Parsing.item]) ""
-- []
-- Parsing.parse (sequence' []) "asdf asdf"
-- [((),"asdf asdf")]
-- Parsing.parse (sequence' [Parsing.failure, Parsing.failure]) "asdf asdf"
-- []
--


-- seems to work
sequence' [] = return []
sequence' (m : ms)
  = m >>=
      \ a ->
        do as <- sequence' ms
           return (a : as)


{-
-- compile error
sequence' ms = foldr func (return ()) ms
  where
	func :: (Monad m) => m a -> m [a] -> m [a]
	func m acc
	  = do x <- m
	       xs <- acc
	       return (x : xs)
-}

{-
-- compile error
sequence' ms = foldr func (return []) ms
  where
	func :: (Monad m) => m a -> m [a] -> m [a]
	func m acc = m : acc
-}

{-
-- parse error
sequence' [] = return []
sequence' (m : ms) = return (a : as)
    where
	a <- m
	as <- sequence' ms
-}

{-
-- seems to work
sequence' ms = foldr func (return []) ms
  where
	func :: (Monad m) => m a -> m [a] -> m [a]
	func m acc
	  = do x <- m
	       xs <- acc
	       return (x : xs)
-}

{-
-- could not compile
sequence' [] = return []
sequence' (m : ms)
  = m >>
      \ a ->
      	do as <- sequence' ms
	   return (a : as)
-}

{-
-- Parse error
sequence' [] = return []
sequence' (ms : ms) = m >>= \ a ->
    as <- sequence' ms
    return (a : as)
-}

{-
-- seems to work
sequence' [] = return []
sequence' (m : ms)
  = do a <- m
       as <- sequence' ms
       return (a : as)
-}


--------------------------------------------

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]

-- takes a non-bottom function of type a -> m b, and a finite,
-- non-partial list of non-bottom elements of type a and (similarly to
-- map) applies the function to every element of the list, but
-- produces the resulting list wrapped inside a monadic action

--
-- test with:
--
-- mapM' (\ a -> do return a) ['a', 'b', 'c']
-- "abc"
-- mapM' (\ a -> return a) ['a', 'b', 'c']
-- "abc"
-- mapM' (\ a -> do putChar a ; return a) ['a', 'b', 'c']
-- abc"abc"
-- mapM' (\ a -> putChar a) ['a', 'b', 'c']
-- abc[(),(),()]
--

-- seems to work
-- mapM' f as = sequence' (map f as)

{-
-- seems to work
mapM' f [] = return []
mapM' f (a : as)
  = f a >>= \ b -> mapM' f as >>= \ bs -> return (b : bs)
-}

{-
-- compile error
mapM' f as = sequence_' (map f as)
-}

{-
-- compile error
mapM' f [] = return []
mapM' f (a : as)
  = f a >> \ b -> mapM' f as >> \ bs -> return (b : bs)
-}

{-
-- parse error
mapM' f [] = return []
mapM' f (a : as) =
    do
	f a -> b
	mapM' f as -> bs
	return (b : bs)
-}

{-
-- seems to work
mapM' f [] = return []
mapM' f (a : as)
  = do b <- f a
       bs <- mapM' f as
       return (b : bs)
-}

{-
-- seems to work
mapM' f [] = return []
mapM' f (a : as)
  = f a >>=
      \ b ->
      	do bs <- mapM' f as
	   return (b : bs)
-}

{-
-- backwards
mapM' f [] = return []
mapM' f (a : as)
  = f a >>=
      \ b ->
      	do bs <- mapM' f as
	   return (bs ++ [b])
-}

--------------------------------------------
--------------------------------------------
--------------------------------------------


