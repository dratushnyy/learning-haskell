{-
 * functions and arguments must begin with  a lower-case letter
 * types should start with upper-case letter

 * use "mild" form of "Hungarian notation"
  xs -> list of "x"
  xss -> list of list of "x"
 * spaces are significant in definitions
 -}
double :: Int -> Int
double x = x + x

quadruple :: Int -> Int
quadruple x = double (double x)

factorial :: Integer -> Integer
factorial n = product [1..n]

average :: [Int] -> Int
-- `` <- makes a function infix
average ns = sum ns `div` length ns











