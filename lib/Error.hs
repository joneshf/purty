module Error
  ( Error
  , unwrap
  ) where

import "rio" RIO

newtype Error
  = Error Utf8Builder

unwrap :: Error -> Utf8Builder
unwrap error' = case error' of
  Error message -> message
