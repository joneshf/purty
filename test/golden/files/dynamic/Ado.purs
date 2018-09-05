module Ado where

foo = ado
  x <- pure 1
  y <- pure 2
  in x + y

bar = ado
  in 12
