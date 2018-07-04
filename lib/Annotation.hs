module Annotation where

import "rio" RIO

data Normalized
  = None
  | Braces
  | Parens

instance Display Normalized where
  display = \case
    None -> "No annotation"
    Braces -> "Braces"
    Parens -> "Parens"

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
