module Main where

import "rio" RIO hiding (withSystemTempFile)

import "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text (putDoc, renderIO)
import "dhall" Dhall                                         (embed, inject)
import "dhall" Dhall.Pretty                                  (prettyExpr)
import "optparse-applicative" Options.Applicative            (execParser)
import "path-io" Path.IO
    ( copyFile
    , copyPermissions
    , withSystemTempFile
    )

import "purty" App   (App)
import "purty" Args  (Args(Args, Defaults, filePath), argsInfo, parseConfig)
import "purty" Env
    ( Config(Config, verbosity)
    , Env(Env)
    , Output(InPlace, StdOut)
    , Verbosity(Verbose)
    , absolutize
    , config
    , defaultConfig
    , defaultPrettyPrintConfig
    , envL
    , logFunc
    , outputL
    , prettyPrintConfig
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
  Args { filePath } -> do
    env <- view envL
    output <- view outputL
    logDebug ("Env: " <> display env)
    logDebug ("Converting " <> display filePath <> " to an absolute path")
    absPath <- absolutize filePath
    logDebug ("Converted file to absolute: " <> displayShow absPath)
    logDebug "Running main `purty` program"
    stream <- Purty.fromAbsFile absPath
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
  Defaults -> liftIO (putDoc $ prettyExpr $ embed inject defaultConfig)
