module RecordPuns where

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
