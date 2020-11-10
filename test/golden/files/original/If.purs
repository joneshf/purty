module If where

foo = if true then 1 else 2

bar = do
  x <- if true
          then pure 1
          else pure 2
  pure x

baz = do
  let x = if true
             then pure 1
             else pure 2
  pure x

qux = [if true then
         1 else 2]

cor = if true then
        1
      else if true then
              2
            else if true then
                  3
                else
                  4

gar = if true then
        1
      else if true then
             ( if true then
                 2 else if true then
                            2 else 2)
           else if true then
                  if true then if true then 3
                                            else if true then 3 else
                                                   3
                  else if true then 3 else 3
                else
                  4
