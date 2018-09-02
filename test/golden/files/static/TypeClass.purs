module TypeClass where

class Foo a

class Bar a where
  bar ::
    a ->
    a ->
    a

class FunDep a b | a -> b

class MultiFunDep a b c d e | b c -> d, d -> b c
