module Kinded where

data Foo (a :: Type)

data Bar = Baz (Boolean :: Type)

data Qux (a :: #Type)
