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
    ( Args(Args, output, verbosity)
    , Env(Env)
    , Output(InPlace, StdOut)
    , Verbosity(Verbose)
    , absolutize
    , argsInfo
    , defaultPrettyPrintConfig
    , envArgs
    , envLogFunc
    , envPrettyPrintConfig
    , purty
    )

main :: IO ()
main = do
  (envArgs@Args{ verbosity, output }, filePath) <- execParser argsInfo
  let envPrettyPrintConfig = defaultPrettyPrintConfig
  logOptions <- logOptionsHandle stderr (verbosity == Verbose)
  withLogFunc logOptions $ \envLogFunc -> do
    let env = Env { envArgs, envLogFunc, envPrettyPrintConfig }
    runRIO env $ do
      logDebug ("Env: " <> display env)
      logDebug "Running main `purty` program"
      stream' <- purty filePath
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
              absPath <- absolutize filePath
              renderIO h stream
              hClose h
              copyPermissions absPath fp
              copyFile fp absPath
            StdOut -> do
              logDebug "Printing to stdout"
              liftIO $ renderIO stdout stream
