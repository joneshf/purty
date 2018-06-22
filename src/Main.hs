module Main where

import "rio" RIO hiding (withSystemTempFile)

import "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import "dhall" Dhall                                         (embed, inject)
import "dhall" Dhall.Pretty                                  (prettyExpr)
import "optparse-applicative" Options.Applicative            (execParser)

import "purty" App   (App)
import "purty" Args  (Args(Args, Defaults, filePath), argsInfo, parseConfig)
import "purty" Env
    ( Config(Config, verbosity)
    , Env(Env, config, logFunc, prettyPrintConfig)
    , Verbosity(Verbose)
    , defaultConfig
    , defaultPrettyPrintConfig
    )
import "purty" Error (Error, errors)

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
    App.run env (program cliArgs `App.handle` errors)

program :: Args -> App Env Error ()
program = \case
  Args { filePath } -> Purty.fromPurtyFilePath filePath
  Defaults -> liftIO (putDoc $ prettyExpr $ embed inject defaultConfig)
