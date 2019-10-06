module FP101x.Lecture03
where
{-
1. Using library functions, define a function halve:: [a] -> ([a], [a]) that splits an even-lengthed list into
two halves. For example
> halve [1,2,3,4,5,6]
([1,2,3], [4,5,6])
Hutton, Graham. Programming in Haskell (p. 45). Cambridge University Press. Kindle Edition.
-}

halve:: [a] -> ([a] , [a])
halve [] = ([], [])
halve xs
  | (length xs `mod` 2 == 0) = (take half xs, drop half xs)
  | otherwise = error "Can not process un even-mengthed list"
  where half = (length xs) `div` 2


{-
2. Define a function third :: [a] -> a that returns the third element in a list that contains
   at least this many elements using: a.​head and tail; b.​list indexing !!; c.​pattern matching.
   Hutton, Graham. Programming in Haskell (p. 45). Cambridge University Press. Kindle Edition.
-}
thirdA :: [a] -> a
thirdA [] = error "Can not get third element on an empty list"
thirdA xs = head ( tail (tail xs))

thirdB :: [a] -> a
thirdB [] = error "Can not get third element on an empty list"
thirdB xs
  | l >= 3 = xs !!2
  where l = length xs

thirdC :: [a] -> a
thirdC [] = error "Can not get third element on an empty list"
thirdC (_:_:x:_) = x


{-
3. Consider a function safetail :: [a] -> [a] that behaves in the same way as tail except that it maps the empty list
 to itself rather than producing an error. Using tail and the function null :: [a] -> Bool that decides if a list is empty or not,
  define safetail using:
  a.​a conditional expression;
  b.​guarded equations;
  c.​pattern matching.

Hutton, Graham. Programming in Haskell (p. 45). Cambridge University Press. Kindle Edition.
-}

safetailA :: [a] -> [a]
safetailA xs =
  if null xs then xs else tail xs

safetailB :: [a] -> [a]
safetailB xs
  | null xs = xs
  | otherwise = tail xs

safetailC :: [a] -> [a]
safetailC [] = []
safetailC (_:xs) = xs



{-
4.In a similar way to && in section 4.4, show how the disjunction operator || can be defined in four different ways using pattern matching.
  Hutton, Graham. Programming in Haskell (p. 45). Cambridge University Press. Kindle Edition.
-}

or' :: Bool -> Bool -> Bool
_ `or'` True = True
True `or'` _ = True
_ `or'` _ = False

{- 5 && using conditional expressions -}
and' :: Bool -> Bool -> Bool
and' a b =
  if a == True then
    if b == True then True else False
  else False

{-6-}
and2:: Bool ->Bool->Bool
and2 a b =
  if a == True then b else False

{-7-}
mult2 :: Integer ->Integer -> Integer ->Integer
--mult x y z = x * y * z
mult2 = \x -> (\y -> (\z -> x * y * z) )

{-8-}
luhnDouble :: Int -> Int
luhnDouble x =
  let d = x * 2
  in
   if d > 9 then d - 9 else d

luhn :: Int->Int->Int->Int->Bool
luhn a b c d =
  let
    total = d + luhnDouble c + b + luhnDouble a
  in
    total `mod` 2 == 0
