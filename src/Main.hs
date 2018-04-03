module Main where

import "protolude" Protolude

import "prettyprinter" Data.Text.Prettyprint.Doc
    ( defaultLayoutOptions
    )
import "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text (renderIO)
import "optparse-applicative" Options.Applicative            (execParser)

import "purty" Purty
    ( Env(Env)
    , PrettyPrintConfig(PrettyPrintConfig)
    , argsInfo
    , envArgs
    , envPrettyPrintConfig
    , layoutOptions
    , purty
    , runPurty
    )

main :: IO ()
main = do
  envArgs <- execParser argsInfo
  let envPrettyPrintConfig =
        PrettyPrintConfig { layoutOptions = defaultLayoutOptions }
  stream' <- runPurty Env { envArgs, envPrettyPrintConfig } purty
  case stream' of
    Left error -> do
      putErrText "Problem parsing module"
      putErrText (show error)
    Right stream -> do
      liftIO $ renderIO stdout stream
