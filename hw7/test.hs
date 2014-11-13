data Expr = Val Int | Div Expr Expr

eval           :: Expr -> Int
eval (Val n)   =  n
eval (Div x y) =  eval x `div` eval y
