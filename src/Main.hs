module Main where

import "rio" RIO

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
  withLogFunc logOptions $ \envLogFunc ->
    runRIO Env { envArgs, envLogFunc, envPrettyPrintConfig } $ do
      stream' <- purty
      case stream' of
        Left err -> do
          logError "Problem parsing module"
          logError (displayShow err)
        Right stream -> do
          liftIO $ renderIO stdout stream
