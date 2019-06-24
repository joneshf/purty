module ParenthesesIndentation where

foreign import kind Foo

foreign import data Bar :: (Foo
                           -> Foo)

instance bazQux :: (Baz a,
                    Baz
                    b) => Baz (Qux a b)
foo :: ( Array
       Int)
foo = ( [1,
             2,3,4])

bar :: Either (Int -> Int)
  (Int ->
  Int) -> Int -> Int
bar x = case x of
  (Left
    y) -> y
  (Right
    z) -> z

baz :: forall f. (Applicative
                 f) => f Boolean
baz = pure true
