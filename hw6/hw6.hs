------------------------------------------------------------------

all :: (a -> Bool) -> [a] -> Bool

-- test with
-- *Main> Main.all (==1) [1, 1, 1]
-- True
-- *Main> Main.all (==1) [1, 1, 2]
-- False

all p xs = and (map p xs)                  -- yes
-- all p xs = map p (and xs)               -- no
-- all p = and . map p                     -- yes
-- all p = not . any (not . p)             -- yes
-- all p = map p . and                     -- no
-- all p xs = foldl (&&) True (map p xs)   -- yes
-- all p xs = foldr (&&) False (map p xs)  -- compiles, but doesn't do what you want
-- all p = foldr (&&) True . map p         -- yes

------------------------------------------------------------------

any :: (a -> Bool) -> [a] -> Bool


-- test with
-- *Main> Main.any (==1) [1, 1, 2]
-- True
-- *Main> Main.any (==1) [2, 2, 2]
-- False


-- any p = map p . or                                  -- no
any p = or . map p                                     -- yes
-- any p xs = length (filter p xs) > 0                 -- yes
-- any p = not . null . dropWhile (not . p)            -- yes
-- any p = null . filter p                             -- compiles but does the wrong thing
-- any p xs = not (Main.all (\ x -> not (p x)) xs)     -- yes
-- any p xs = foldr(\ x acc -> (p x) || acc) False xs  -- yes
-- any p xs = foldr (||) True (map p xs)               -- compiles but does the wrong thing

------------------------------------------------------------------

takeWhile :: (a -> Bool) -> [a] -> [a]

-- test with
-- *Main> Main.takeWhile (==1) [2, 2, 2]
-- []
-- *Main> Main.takeWhile (==1) [1, 2, 2]
-- [1]
-- *Main> Main.takeWhile (==1) [1..]
-- [1]
-- *Main> Main.takeWhile (==1) [1, 2, 1]
-- [1]


-- no:
-- takeWhile _ [] = []
-- takeWhile p (x : xs)
--   | p x = x : Main.takeWhile p xs
--   | otherwise = Main.takeWhile p xs

-- yes:
takeWhile _ [] = []
takeWhile p (x : xs)
  | p x = x : Main.takeWhile p xs
  | otherwise = []

-- no:
-- takeWhile _ [] = []
-- takeWhile p (x : xs)
--   | p x = Main.takeWhile p xs
--   | otherwise = []

-- no -- this seems to do the right thing, but it doesn't stop early,
-- so it doesn't work on [1, 2, 1]
--
-- takeWhile p = foldl (\ acc x -> if p x then x : acc else acc) []

------------------------------------------------------------------

dropWhile :: (a -> Bool) -> [a] -> [a]

-- test with
-- *Main> Main.dropWhile (==1) [2, 2, 2]
-- [2, 2, 2]
-- *Main> Main.dropWhile (==1) [1, 2, 2]
-- [2, 2]
-- *Main> Main.dropWhile (==1) [1, 2, 1]
-- [2, 1]
-- *Main> Main.dropWhile (==1) [1, 1, 1]
-- []

-- yes:
dropWhile _ [] = []
dropWhile p (x : xs)
  | p x = Main.dropWhile p xs
  | otherwise = x : xs

-- no:
-- dropWhile _ [] = []
-- dropWhile p (x : xs)
--   | p x = Main.dropWhile p xs
--   | otherwise = xs

-- no, doesn't stop early, it's basically like filter
-- dropWhile p = foldr (\ x acc -> if p x then acc else x : acc) []

-- no, it reverses the output
-- dropWhile p = foldl add []
--   where add [] x = if p x then [] else [x]
--         add acc x = x : acc


------------------------------------------------------------------
------------------------------------------------------------------
