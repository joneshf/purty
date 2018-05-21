module Main where

import "rio" RIO hiding (withSystemTempFile)

import "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text (renderIO)
import "optparse-applicative" Options.Applicative            (execParser)
import "path-io" Path.IO
    ( copyFile
    , copyPermissions
    , withSystemTempFile
    )
import "base" System.Exit                                    (exitFailure)

import "purty" Purty
    ( Args(Args, argsFilePath)
    , Config(Config, output, verbosity)
    , Env(Env)
    , Output(InPlace, StdOut)
    , Verbosity(Verbose)
    , absolutize
    , argsInfo
    , defaultPrettyPrintConfig
    , envConfig
    , envLogFunc
    , envPrettyPrintConfig
    , parseConfig
    , purty
    )

main :: IO ()
main = do
  cliArgs@Args { argsFilePath } <- execParser argsInfo
  envConfig@Config{ verbosity, output } <- parseConfig cliArgs
  let envPrettyPrintConfig = defaultPrettyPrintConfig
  logOptions <- logOptionsHandle stderr (verbosity == Verbose)
  withLogFunc logOptions $ \envLogFunc -> do
    let env = Env { envConfig, envLogFunc, envPrettyPrintConfig }
    runRIO env $ do
      logDebug ("Env: " <> display env)
      logDebug ("Converting " <> display argsFilePath <> " to an absolute path")
      absPath <- absolutize argsFilePath
      logDebug ("Converted file to absolute: " <> displayShow absPath)
      logDebug "Running main `purty` program"
      stream' <- purty absPath
      case stream' of
        Left err -> do
          logError "Problem parsing module"
          logError (displayShow err)
          liftIO exitFailure
        Right stream -> do
          logDebug "Successfully created stream for rendering"
          logDebug (displayShow $ void stream)
          case output of
            InPlace -> liftIO $ withSystemTempFile "purty.purs" $ \fp h -> do
              renderIO h stream
              hClose h
              copyPermissions absPath fp
              copyFile fp absPath
            StdOut -> do
              logDebug "Printing to stdout"
              liftIO $ renderIO stdout stream
