module Purty
  ( format
  , run
  ) where

import "rio" RIO hiding (log)

import qualified "this" Args
import qualified "this" Error
import qualified "this" Log

format :: Log.Handle -> FilePath -> IO (Either Error.Error Utf8Builder)
format _log _file = pure (Right "")

run :: Log.Handle -> Args.Args -> IO (Maybe Error.Error)
run _log _args = pure Nothing
