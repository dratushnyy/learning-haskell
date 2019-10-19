
--------------
-- 1.
sumToHundred = let l = [x^2| x<- [1..100]] in foldl (+) 0 l

-- 2.
grid :: Int -> Int -> [(Int,Int)]
grid m n = [(m', n') | m' <- [0..m], n' <- [0..n]]

-- 3.
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- 4
replicate' :: Int -> a -> [a]
replicate' x a = [a | _ <- [1 .. x]]

-- 5
pyths :: Int -> [(Int, Int, Int)]
pyths i = [(x, y, z) | x <- [1..i], y <- [1..i], z <- [1..i], x^2 + y^2 == z^2]

-- 6
perfect :: Int -> [Int]
perfect i = [x | x <- [1..i], x == sum (filter (\n -> n /= x ) (factors x)) ]

-- 7

-- 8
position' :: Eq a=> a->[a] -> [Int]
position' a as =
  let t = zip as [0 .. l'] in find a t
  where l' = (length as ) - 1

-- 9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = let p = zip xs ys in sum [x*y | (x,y) <- p]