-- list comprehensions
concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

firsts :: [(a, b)] -> [a]
firsts ps = [x | (x, _) <- ps]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]
-- naive prime
prime :: Int -> Bool
prime n = factors n == [1, n]

find :: Eq k => k -> [(k, v)] -> [v]
find k t = [v | (k', v) <- t, k == k']

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
-- note that [1..] is generates infinite list
-- but it will stop once "zip" exhausts "xs"
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

