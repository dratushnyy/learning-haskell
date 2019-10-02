bmiTell:: (RealFloat a) => a->String
bmiTell bmi
  | bmi <= 18.5 = "You're underweight, you emou, you!"
  | bmi <=25 = "You're supposedely normal.Pfft, I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're are whale, congratulations"


bmiTell2 :: (RealFloat a) => a->a->String
bmiTell2 weight height -- no  = after params and before guards
  | weight / height ^ 2 <= 18.5 = "You're underweight, you emou, you!"
  | weight / height ^ 2 <=25 = "You're supposedely normal.Pfft, I bet you're ugly!"
  | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're are whale, congratulations"


max' :: (Ord a) => a->a->a
max' a b
  | a > b = a
  | otherwise = b

compare' :: (Ord a) => a->a->Ordering
-- it is possible not only to use function with backticks, but define it as well
a `compare'` b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT