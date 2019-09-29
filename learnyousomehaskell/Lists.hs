-- S = {2 * x | x from N, x < 10}
input :: Int -> Int -> [Int]
input from to = [from .. to]

predicate :: Int -> Bool
predicate x =
  let square = x * 2
  in square > 12

s = [2 * x | x <- input 5 25, predicate x]