module DoAndIfThenElse where

foo = do if true then 0 else 1

bar = do if true then 0 else
           1

baz = do if true then do 0 else do
           x <- pure 1
           y <- pure 0
           pure (x + y)

qux = do if true then do
           x <- pure 0
           y <- pure 0
           pure (x + y)
           else do 1
