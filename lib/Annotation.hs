module Annotation where

import "rio" RIO

import qualified "this" Log

data Normalized
  = None
  | Braces
  | Parens
  deriving (Show)

instance Log.Inspect Normalized

data Sorted
  = Sorted
  deriving (Show)

instance Log.Inspect Sorted

data Unannotated
  = Unannotated
  deriving (Show)

instance Log.Inspect Unannotated
