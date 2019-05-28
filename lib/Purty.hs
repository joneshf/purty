module Purty
  ( format
  , run
  ) where

import "rio" RIO hiding (log)

import qualified "this" Args
import qualified "this" Error
import qualified "this" Log

format :: Log.Handle -> LByteString -> IO (Either Error.Error Utf8Builder)
format _log _file = pure (Right "")

run :: Log.Handle -> Args.Args -> IO (Maybe Error.Error)
run log args = case args of
  Args.Format format' -> do
    results <- Args.withInput log format' (format log)
    case results of
      Left err -> do
        Log.debug log "Error formatting file"
        pure (Just err)
      Right formatted -> do
        Log.debug log "Writing formatted file"
        Args.write log format' formatted
        pure Nothing
