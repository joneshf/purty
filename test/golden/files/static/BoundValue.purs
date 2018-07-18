module BoundValue where

newtype Foo a
  = Foo a

foo ::
  Int
foo = x
  where
  Foo x = let Foo y = Foo (Foo 1)
          in y
