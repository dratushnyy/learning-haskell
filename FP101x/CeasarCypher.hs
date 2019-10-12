import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let i = chr (ord 'a' + i)

shift :: Char -> Int -> Char
shift c i
  | isLower c = int2let ((let2int c + i) `mod` 26 )
  | otherwise = c

encode :: String -> Int ->String
encode cs i = [shift c i| c <- cs]

percent :: Int->Int->Float
percent n m = (fromIntegral n / fromIntegral m) * 100

count :: Char -> String -> Int
count c cs =
  length [c' | c' <- cs, c' == c ]

lowers :: String -> Int
lowers cs =
  length [c | c <- cs, c >= 'a' && c <= 'z']

freqs :: String -> [Float]
freqs cs =
  [percent (count c cs) n | c <- ['a'..'z']]
  where n = lowers cs

chisqr :: [Float] -> [Float] -> Float
chisqr os es =
  sum [((o-e) ^ 2) / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

crack :: String -> String
crack xs = encode xs (-factor)
    where
        factor = head (positions (minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n <- [0..25]]
        table' = freqs xs