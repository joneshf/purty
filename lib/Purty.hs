{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Purty
  ( format,
    run,
  )
where

import qualified "this" Annotation
import qualified "this" Args
import qualified "componentm" Control.Monad.Component
import qualified "this" Error
import qualified "this" Format
import qualified "purescript-cst" Language.PureScript.CST.Errors
import qualified "purescript-cst" Language.PureScript.CST.Parser
import qualified "purs-tool-log" Log
import "rio" RIO hiding (log)
import qualified "rio" RIO.NonEmpty

format :: Log.Handle -> LByteString -> IO (Either Error.Error Utf8Builder)
format log contents' = do
  Log.debug log "Decoding contents to `Text`."
  case decodeUtf8' (toStrictBytes contents') of
    Left err ->
      pure (Left $ Error.new $ "Error decoding contents" <> displayShow err)
    Right decoded -> do
      Log.debug log ("Parsing contents: " <> display decoded)
      case Language.PureScript.CST.Parser.parse decoded of
        Left err' ->
          pure
            ( Left
                $ Error.new
                $ "Error parsing contents:"
                  <> foldMap
                    ( \err ->
                        newline
                          <> indentation
                          <> fromString (Language.PureScript.CST.Errors.prettyPrintError err)
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
    case RIO.NonEmpty.nonEmpty result of
      Just errs -> do
        for_ errs $ \err -> do
          Log.debug log (Error.format err)
          Log.info log (Error.message err)
        pure (ExitFailure 1)
      Nothing -> pure ExitSuccess
  where
    config :: Log.Config
    config =
      Log.Config
        { Log.name = "Log - main program",
          Log.verbose = Args.debug args
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

run' :: Log.Handle -> Args.Args -> IO [Error.Error]
run' log args = case args of
  Args.Args mode _ -> case mode of
    Args.Format format' -> do
      Log.debug log "Formatting input"
      Args.withInput log format' (format log)
    Args.Validate validate' -> do
      Log.debug log "Validating input"
      Args.withValidate log validate' (format log)
    Args.Version version' -> do
      Log.debug log "Displaying version information"
      Args.writeVersion log version'
      pure []
