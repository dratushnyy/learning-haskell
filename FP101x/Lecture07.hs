-- higher order functions
mapLC :: (a -> b) -> [a] -> [b]
mapLC f xs =  [f x | x <- xs]

mapRec :: (a -> b) -> [a] -> [b]
mapRec f [] = []
mapRec f (x:xs) = f x : map f xs

filterLC :: (a -> Bool) -> [a] -> [a]
filterLC p xs = [x | x <- xs, p x]

filterRec :: (a -> Bool) -> [a] -> [a]
filterRec p []            = []
filterRec p (x:xs) | p  x = x: filter p xs
                | otherwise = filter p xs