module RecordPuns where

import Foo
  (name) 
import Foo as Foo

foo ::
  { age :: Int, name :: String } ->
  String
foo {age, name} = name <> " is " <> show age <> " years old"

bar ::
  { age :: Int, name :: String } ->
  { age :: Int, name :: String }
bar {age, name} = { age: age + 1
                  , name
                  }

baz ::
  { age :: Int, name :: String } ->
  { age :: Int, name :: String }
baz {age, name} = { age: Foo.age
                  , name
                  }

qux ::
  { age :: Int } ->
  { age :: Int, name :: String }
qux {age} = {age: Foo.age, name}
