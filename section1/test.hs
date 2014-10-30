--
-- start ghci with this script like:
--
--   ghci <path to script>
--
-- then reload with
--
--   :reload
--
-- (no arguments)
--
--

double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

--
-- the ` ` changes div from a prefix operator to an infix operator
--

average ns = sum ns `div` length ns

-- variables and functions must start with lower case
-- types must start with upper case

-- by convention, lists end in s, lists of lists end in ss
-- usually haskell identifiers are short

-- whitespace is significant, each definition must begin in the same
-- column


--
-- Useful GHCi commands
--
-- :load <script>
-- :reload           reload current script
-- :edit <script>    edit given script
-- :edit             edit current script
-- :type <expr>      get the type of something
-- :?                show all commands
-- :quit
--
