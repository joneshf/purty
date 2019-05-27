module ParenthesizedFunctions where

x :: (Int -> Int) -> Int
x f = f 1

y ::
  forall expected r unexpected.
  (forall a. Show a => a -> {expected :: expected, unexpected :: unexpected | r}) ->
  Boolean ->
  {expected :: expected, unexpected :: unexpected}
y f x = { expected, unexpected }
  where
  expected = result.expected
  result = f x
  unexpected = result.unexpected
