module Ado where

foo = ado
  x <-
    pure 1
  y <- do
       pure 2
       pure 2
  z <- do pure 3
  in x + y + z

bar = ado in 12
