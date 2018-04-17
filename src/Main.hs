module Main where

import "rio" RIO

import "base" System.Exit (exitFailure)

import "prettyprinter" Data.Text.Prettyprint.Doc
    ( defaultLayoutOptions
    )
import "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text (renderIO)
import "optparse-applicative" Options.Applicative            (execParser)

import "purty" Purty
    ( Args(Args, verbose)
    , Env(Env)
    , PrettyPrintConfig(PrettyPrintConfig)
    , argsInfo
    , envArgs
    , envLogFunc
    , envPrettyPrintConfig
    , layoutOptions
    , purty
    )

main :: IO ()
main = do
  envArgs@Args{ verbose } <- execParser argsInfo
  let envPrettyPrintConfig =
        PrettyPrintConfig { layoutOptions = defaultLayoutOptions }
  logOptions <- logOptionsHandle stderr verbose
  withLogFunc logOptions $ \envLogFunc -> do
    let env = Env { envArgs, envLogFunc, envPrettyPrintConfig }
    runRIO env $ do
      logDebug ("Env: " <> display env)
      logDebug "Running main `purty` program"
      stream' <- purty
      case stream' of
        Left err -> do
          logError "Problem parsing module"
          logError (displayShow err)
          liftIO exitFailure
        Right stream -> do
          logDebug "Successfully created stream for rendering"
          logDebug (displayShow $ void stream)
          liftIO $ renderIO stdout stream
