{-
  'where' let bind variables at the end of a function.
  Whole function can see them, including all the guards.

  expression
  expression
  expression
  ...
  where
      binding
      binding
      binding
      ...
-}
bmiTell :: (RealFloat a) => a->a->String
bmiTell weight height
  | bmi <= skinny = "You're underweight, you emou, you!"
  | bmi <=normal = "You're supposedely normal.Pfft, I bet you're ugly!"
  | bmi <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're are whale, congratulations"
  where
    bmi = weight/ height ^ 2
    -- pattern matching is also working
    (skinny, normal, fat) = (18.5, 25.0, 30.0)


initials :: String->String->String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname