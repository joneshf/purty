module Typed where

foo (x ::          Boolean) = x

bar = \(x         :: Boolean) -> x

baz = true :: Boolean
