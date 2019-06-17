module Purty
  ( format
  , run
  ) where

import "rio" RIO hiding (log)

import qualified "this" Annotation
import qualified "this" Args
import qualified "componentm" Control.Monad.Component
import qualified "this" Error
import qualified "this" Format
import qualified "purescript" Language.PureScript.CST
import qualified "this" Log

format :: Log.Handle -> LByteString -> IO (Either Error.Error Utf8Builder)
format log contents' = do
  Log.debug log "Decoding contents to `Text`."
  case decodeUtf8' (toStrictBytes contents') of
    Left err ->
      pure (Left $ Error.new $ "Error decoding contents" <> displayShow err)
    Right decoded -> do
      Log.debug log ("Parsing contents: " <> display decoded)
      case Language.PureScript.CST.parse decoded of
        Left err' ->
          pure
            ( Left
              $ Error.new
              $ "Error parsing contents:"
              <> foldMap
                (\err ->
                  newline
                    <> indentation
                    <> fromString (Language.PureScript.CST.prettyPrintError err)
                )
                err'
            )
        Right parsed -> do
          Log.debug log ("Parsed contents: " <> displayShow parsed)
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

run :: Args.Args -> IO ExitCode
run args =
  runComponent (Args.debug args) "purty" (Log.handle config) $ \log -> do
    result <- run' log args
    case result of
      Just err -> do
        Log.debug log (Error.format err)
        Log.info log (Error.message err)
        pure (ExitFailure 1)
      Nothing -> pure ExitSuccess
  where
  config :: Log.Config
  config =
    Log.Config
      { Log.name = "Log - main program"
      , Log.verbose = Args.debug args
      }

  runComponent ::
    Bool ->
    Text ->
    Control.Monad.Component.ComponentM a ->
    (a -> IO ExitCode) ->
    IO ExitCode
  runComponent debug
    | debug =
      Control.Monad.Component.runComponentM1 (runSimpleApp . logInfo . display)
    | otherwise =
      Control.Monad.Component.runComponentM

run' :: Log.Handle -> Args.Args -> IO (Maybe Error.Error)
run' log args = case args of
  Args.Defaults defaults -> do
    Log.debug log "Outputting defaults"
    Args.writeDefaults log defaults
    pure Nothing
  Args.Format format' -> do
    Log.debug log "Formatting input"
    results <- Args.withInput log format' (format log)
    case join results of
      Left err ->
        pure (Just $ Error.wrap ("Error formatting " <> Args.input format') err)
      Right formatted -> do
        Log.debug log "Writing formatted module."
        Args.write log format' formatted
        pure Nothing
