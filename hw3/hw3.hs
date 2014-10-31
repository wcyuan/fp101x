-------------------------------------------------------------------

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

safetail1 xs = if null xs then [] else tail xs

safetail2 [] = []
safetail2 (_ : xs) = xs

