module Ado where

foo = ado
  let w = 0
  x <- pure 1
  y <- do
    pure 2
    pure 2
  z <- do pure 3
  in w + x + y + z

bar = ado in 12
