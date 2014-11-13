import Parsing

-- Run in ghci as
-- :load ".../hw7/hw7.hs" ".../hw7/parsing.lhs"

-- nat :: Parser Int
-- nat
--   = do xs <- many1 digit
--        return (read xs)

-----------------------------------------------

-- test like this:
--
-- parse Main.int "-004"
-- [(-4,"")]

-- int = (nat +++ char '-') >>= (\ c -> nat >>= (\ n -> return (-n)))

int
  = (do char '-'
        n <- nat
        return (-n))
     +++ nat

-----------------------------------------------

-- test like this:
--
-- parse Main.comment "-- asdf asdf\nasdf"
-- [((),"\nasdf")]
--

comment
  = do string "--"
       many (sat (/= '\n'))
       return ()

-- this doesn't work
-- comment
--   = do string "--"
--        sat (== '\n')
--        return ()

-----------------------------------------------
-- test it like this:
--
-- parse Main.expr "5 - 10 - 1000"
-- [(-1005,"")]

-- Given "X - Y - Z" it returns [(X - Y), "- Z"]
-- expr
--   = do n <- natural
--        symbol "-"
--        n' <- natural
--        return (n - n')

-- doesn't compile
-- expr
--   = do n <- natural
--        ns <- many (do symbol "-"
--                       natural)

-- always returns empty?
-- expr
--   = do n <- natural
--        symbol "-"
--        e <- Main.expr
--        return (e - n)


expr
  = do n <- natural
       ns <- many
               (do symbol "-"
                   natural)
       return (foldl (-) n ns)

