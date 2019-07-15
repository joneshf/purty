module If where

foo = if true then 1 else 2

bar = do
  x <-
    if true then
      pure 1
    else
      pure 2
  pure x

baz = do
  let
    x =
      if true then
        pure 1
      else
        pure 2
  pure x

qux =
  [ if true then
      1
    else
      2
  ]
