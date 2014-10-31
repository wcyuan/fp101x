second xs = head (tail xs)
pair x y = (x, y)
double x = x * 2
palindrome xs = reverse xs == xs
twice f x = f (f x)
f xs = take 3 (reverse xs)
