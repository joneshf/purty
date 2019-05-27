module GuardedValue where

foo x
  | x >= 0, x > 1 = "positive"
  | x > 0 = "zero"
  | otherwise = "negative"
