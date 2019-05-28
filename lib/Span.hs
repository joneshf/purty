module Span
  ( Span(..)
  ) where

import "rio" RIO

data Span
  = MultipleLines
  | SingleLine
  deriving (Show)
