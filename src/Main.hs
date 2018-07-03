module Main where

import "rio" RIO

import "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import "dhall" Dhall                                         (embed, inject)
import "dhall" Dhall.Pretty                                  (prettyExpr)
import "optparse-applicative" Options.Applicative            (execParser)

import "purty" Args  (Args(Args, Defaults, filePath), argsInfo, parseConfig)
import "purty" Env
    ( Config(Config, verbosity)
    , Env(Env, config, logFunc, prettyPrintConfig)
    , Verbosity(Verbose)
    , defaultConfig
    , defaultPrettyPrintConfig
    )
import "purty" Error (errors)

import qualified "purty" App
import qualified "purty" Purty

main :: IO ()
main = do
  cliArgs <- execParser argsInfo
  config@Config{ verbosity } <- parseConfig cliArgs
  let prettyPrintConfig = defaultPrettyPrintConfig
  logOptions <- logOptionsHandle stderr (verbosity == Verbose)
  withLogFunc logOptions $ \logFunc -> do
    let env = Env { config, logFunc, prettyPrintConfig }
    App.run env $ case cliArgs of
      Args { filePath } -> Purty.fromPurtyFilePath filePath `App.handle` errors
      Defaults -> liftIO (putDoc $ prettyExpr $ embed inject defaultConfig)
