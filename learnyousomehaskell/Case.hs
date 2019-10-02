{-
  Pattern matching on parameters in function definitions
  is a syntactic sugar for case expressions


  case expression of
      pattern -> result
      pattern -> result
      pattern -> result
      ....
  'expression' is matched against the patterns.
-}

head':: [a] -> a
head' [] = error "No head for empty list!"
head' (x:_) = x

head'' :: [a] -> a
head'' xs = case xs of
  [] -> error "No head for empty list!"
  (x:_) -> x


describeList :: [a] -> String
describeList xs =
  "The list is " ++
   case xs of
          [] -> "empty."
          [x] -> "a singleton list."
          xs -> "a longer list"


describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
  where what [] = "emtpy."
        what [x] = "singleton list."
        what xs = "a longer list"
