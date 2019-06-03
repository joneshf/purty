module Operators where

foo = filter ((&&) <$> testK relation w <*> evaluate' x1) worlds

bar = ((&&) <$> (||) <*> (&&))
