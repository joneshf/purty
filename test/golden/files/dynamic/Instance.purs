module Instance where

data Baz
  

instance fooBaz :: Foo Baz
instance barBaz :: Bar Baz where
  bar = append
  