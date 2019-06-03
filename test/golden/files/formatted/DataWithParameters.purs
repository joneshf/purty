module DataWithParameters where

data Foo a
  = Foo

data Bar a b c
  = Bar a -- This is a comment
  | Baz Int
  | Qux (Foo a) (Bar a b c)

data Gar a b
  = Gar a
  | Ply b
