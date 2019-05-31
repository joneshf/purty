module Purty
  ( format
  , run
  ) where

import "rio" RIO hiding (log)

import qualified "this" Annotation
import qualified "this" Args
import qualified "this" Error
import qualified "this" Format
import qualified "purescript" Language.PureScript.CST
import qualified "this" Log

format :: Log.Handle -> LByteString -> IO (Either Error.Error Utf8Builder)
format log contents' = do
  Log.debug log "Decoding to file contents to `Text`."
  case decodeUtf8' (toStrictBytes contents') of
    Left err ->
      pure (Left $ Error.new $ "Error decoding file contents" <> displayShow err)
    Right decoded -> do
      Log.debug log ("Parsing file. Contents: " <> display decoded)
      case Language.PureScript.CST.parse decoded of
        Left err' ->
          pure
            ( Left
              $ Error.new
              $ "Error parsing file:"
              <> foldMap
                (\err ->
                  newline
                    <> indentation
                    <> fromString (Language.PureScript.CST.prettyPrintError err)
                )
                err'
            )
        Right parsed -> do
          Log.debug log ("Parsed file: " <> displayShow parsed)
          Log.debug log "Annotating module"
          annotated <- Annotation.module' log parsed
          Log.debug log ("Annotated module" <> displayShow annotated)
          Log.debug log "Formatting module"
          formatted <- Format.module' log indentation annotated
          Log.debug log ("Formatted module" <> display formatted)
          pure (Right formatted)

indentation :: Utf8Builder
indentation = "  "

newline :: Utf8Builder
newline = "\n"

run :: Log.Handle -> Args.Args -> IO (Maybe Error.Error)
run log args = case args of
  Args.Format format' -> do
    Log.debug log "Formatting file"
    results <- Args.withInput log format' (format log)
    case results of
      Left err -> pure (Just $ Error.wrap "Error formatting file" err)
      Right formatted -> do
        Log.debug log "Writing formatted file."
        Args.write log format' formatted
        pure Nothing
