{-
 * functions and arguments must begin with  a lower-case letter
 * types should start with upper-case letter

 * use "mild" form of "Hungarian notation"
  xs -> list of "x"
  xss -> list of list of "x"
 * spaces are significant in definitions
 -}
double x = x + x
quadruple x = double (double x)

factorial n = product [1..n]
-- `` <- makes a function infix
average ns = sum ns `div` length ns











