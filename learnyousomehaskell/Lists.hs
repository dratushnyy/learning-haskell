-- S = {2 * x | x from N, x < 10}
listOfInts :: Int -> Int -> [Int]
listOfInts from to = [from .. to]

predicate :: Int -> Bool
predicate x =
  let square = x * 2
  in square > 12

predicate' :: Int -> Int -> Bool
predicate' x y = let r = x `rem` y in r == 0

s = [2 * x | x <- listOfInts 5 25, predicate x]
s' = [x | x <- listOfInts 0 100, predicate x, predicate' x 3]

nouns = ["hobo","frog","pope"]
adjectives = ["lazy", "grouchy", "scheming"]
l = [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]

length' xs = sum [1| _ <-xs]
t = length' (listOfInts 1 100)

triangles = [(a,b,c) | c <- [1..10], b <-[1..c], a <- [1..b], a^2 + b^2 == c^2]