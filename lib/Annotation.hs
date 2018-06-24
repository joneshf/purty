module Annotation where

import "rio" RIO

data Sorted
  = Sorted
  deriving (Show)

instance Display Sorted where
  display = \case
    Sorted -> "Sorted"

data Unannotated
  = Unannotated

instance Display Unannotated where
  display = \case
    Unannotated -> "Unannotated"
