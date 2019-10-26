fac::Int -> Int
fac 0 = 1
fac n
  | n < 0 = error "Factorial is undefined for negative numbers"
  | otherwise = n * fac (n - 1)

product' :: Num a => [a] -> a
product' [] = 1
product' (n:ns) = n * product' ns

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse xs ++ [x]

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  | x <= y = x:y:ys
  | otherwise = y: insert x ys

insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)


sumdown :: (Num a, Eq a) => a -> a
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

(^) :: Int -> Int -> Int
a ^ 0 = 1
a ^ 1 = a
a ^ b = a * (a Main.^ (b - 1))


euclid :: Int -> Int -> Int
euclid x y
  | x == y = x
  | x < y = euclid x (y - x)
  | x > y = euclid (x - y) y

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_:xs) = drop' (n - 1) xs

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = [x] ++ take' (n - 1) xs

init' :: [a] -> [a]
init' [_] = []
init' (x:xs)
 | null xs = []
 | otherwise = x: (init' xs)


and' :: [Bool] -> Bool
and' [] = error "Undefined for an empty list"
and' [True] = True
and' [False] = False
and' (x:xs)
  | x == False = False
  | otherwise = and' xs

mergeLists :: [a] -> [a] ->[a]
mergeLists [] [] = []
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists xs (y:ys) = mergeLists (xs ++ [y]) ys

concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' x c = [c] ++ replicate' (x - 1) c

(!!) :: [a] -> Int -> a
[] !! _ = error "Undefined for empty list"
(x:xs) !! 0 = x
(x:xs) !! n = xs Main.!! (n - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
  | a == x = True
  | otherwise = elem' a xs


merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys)
  | x <= y = x:merge xs (y:ys)
  | otherwise = y:merge (x:xs) ys


halve :: [a] -> ([a], [a])
halve xs =
  let
    halfLength = (length' xs ) `div` 2
  in ((take' halfLength xs), (drop' halfLength xs))


msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs =
  let (left, right) = halve xs
    in merge (msort left) (msort right)