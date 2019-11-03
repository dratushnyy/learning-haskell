
-- Problem 1.
-- Find the last element of a list

myLast :: [a] -> a
myLast [] = error "Undefined for an empty list"
myLast [x] = x
myLast (_:xs) = myLast xs


-- Problem 2.
-- Find the last but one element of a list

myButLast :: [a] -> a
myButLast [] = error "Undefinded for an empty list"
myButLast [x] = error "List length should be greater than 1"
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs


-- Problem 3.
-- Find the K'th element of a list. The first element in the list is number 1.

elementAt :: [a] -> Int -> a
elementAt [] _ = error "Error: Index out of range"
elementAt (x:_) 1 = x
elementAt (x:xs) n
  | n < 1 = error "Error: Index out of range"
  | otherwise = elementAt xs (n - 1)


-- Problem 4
-- Find the number of elements of a list
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myLength' :: [a] -> Int
myLength' = foldr (\_ acc -> acc + 1) 0


-- Problem 5
-- Reverse a list
myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x:acc) []

myReverse' :: [a] -> [a]
myReverse' [] = []
myReverse' (x:xs) = myReverse' xs ++ [x]