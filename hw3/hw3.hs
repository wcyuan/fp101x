-------------------------------------------------------------------
-- halve takes an even length lists and makes it into two equal length lists

-- doesn't work, can't use / on an integer
-- halve1 xs = (take n xs, drop n xs)
--   where n = length xs / 2

halve2 xs = splitAt (length xs `div` 2) xs

halve3 xs = (take (n `div` 2) xs, drop (n `div` 2) xs)
  where n = length xs

-- doesn't work, needs xs at the end
halve4 xs = splitAt (length xs `div` 2)

-- doesn't work, drops the first element after the halfway
halve5 xs = (take n xs, drop (n + 1) xs)
  where n = length xs `div` 2

halve6 xs = splitAt (div (length xs) 2) xs

-- doesn't work, can't use / on an integer
-- halve7 xs = splitAt (length xs / 2) xs

halve8 xs = (take n xs, drop n xs)
  where n = length xs `div` 2

-------------------------------------------------------------------
-- safetail is just like tail except it maps the empty list to the empty list
-- insetad of throwing an error

safetail1 xs = if null xs then [] else tail xs

safetail2 [] = []
safetail2 (_ : xs) = xs

-- fails on empty: non-exhaustive pattern
-- even when not empty, skips the first two, not just the first one
safetail3 (_ : xs)
  | null xs = []
  | otherwise = tail xs

safetail4 xs
  | null xs = []
  | otherwise = tail xs

-- doesn't work, patterns are matched in order
-- this gives a compiler error when loading the file too
safetail5 xs = tail xs
safetail5 [] = []

safetail6 [] = []
safetail6 xs = tail xs

-- doesn't work on empty.  also, wrong return value when there is one element
safetail7 [x] = [x]
safetail7 (_ : xs) = xs

safetail8
  = \ xs ->
      case xs of
          [] -> []
          (_ : xs) -> xs

-------------------------------------------------------------------


