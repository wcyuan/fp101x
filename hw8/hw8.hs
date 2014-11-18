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
mapM' f as = sequence' (map f as)

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

filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]

-- takes a "predicate" of type Monad m => a -> m Bool and uses this to
-- filter a finite, non-partial list of non-bottom elements of type a.

{-
-- seems to return everything
filterM' _ [] = return []
filterM' p (x : xs)
  = do flag <- p x
       ys <- filterM' p xs
       return (x : ys)
-}

-- seems to work
filterM' _ [] = return []
filterM' p (x : xs)
  = do flag <- p x
       ys <- filterM' p xs
       if flag then return (x : ys) else return ys

{-
-- does not compile
filterM' _ [] = return []
filterM' p (x : xs)
  = do ys <- filterM' p xs
       if p x then return (x : ys) else return ys
-}

{-
-- reverses the predicate
filterM' _ [] = return []
filterM' p (x : xs)
  = do flag <- p x
       ys <- filterM' p xs
       if flag then return ys else return (x : ys)
-}

--------------------------------------------

foldLeftM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a

-- takes an accumulation function a -> b -> m a, and a seed of type a
-- and left folds a finite, non-partial list of non-bottom elements of
-- type b into a single result of type m a

{-
-- for reference,
-- here is the definition of foldl
foldl f z []     = z                  
foldl f z (x:xs) = foldl f (f z x) xs
-}

foldLeftM f a [] = return a
foldLeftM f a (x:xs)
  = (f a x) >>= \ b -> foldLeftM f b xs

-- what is the result of:
--
-- foldLeftM (\a b -> putChar b >> return (b : a ++ [b])) [] "haskell" >>= \r -> putStrLn r
-- haskelllleksahhaskell

--------------------------------------------

foldRightM :: Monad m => (a -> b -> m b) -> b -> [a] -> m b

{-
-- for reference, here is the definition of foldr
foldr f z []     = z 
foldr f z (x:xs) = f x (foldr f z xs) 
-}

foldRightM f a [] = return a
foldRightM f a (x:xs)
  = foldRightM f a xs >>= \ b -> (f x b)

-- foldRightM (\a b -> putChar a >> return (a : b)) [] (show [1,3..10]) >>= \r -> putStrLn r
-- ]9,7,5,3,1[[1,3,5,7,9]

--------------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b

-- takes a function of type a -> b and "maps" it over a non-bottom
-- monadic value of type m a to produce a value of type m b?

-- test with:
-- liftM (\ a -> True) (return 1)
-- True
-- liftM (+2) (return 1)
-- 3

{-
-- seems to work
liftM f m
  = do x <- m
       return (f x)
-}

-- does not compile
-- liftM f m = m >>= \ a -> f a

-- seems to work
liftM f m = m >>= \ a -> return (f a)

-- does not compile
-- liftM f m = return (f m)

-- seems to work -- no, it was marked wrong
-- liftM f m = m >>= \ a -> m >>= \ b -> return (f a)

-- seems to work -- no, it was marked wrong
-- liftM f m = m >>= \ a -> m >>= \ b -> return (f b)

-- does not compile
-- liftM f m = mapM f [m]

-- does not compile
-- liftM f m = m >> \ a -> return (f a)


--------------------------------------------
--------------------------------------------
--------------------------------------------


