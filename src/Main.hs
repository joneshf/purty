module Main where

import "rio" RIO hiding (handle, withSystemTempFile)

import "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text (putDoc, renderIO)
import "dhall" Dhall                                         (embed, inject)
import "dhall" Dhall.Pretty                                  (prettyExpr)
import "optparse-applicative" Options.Applicative            (execParser)
import "path-io" Path.IO
    ( copyFile
    , copyPermissions
    , withSystemTempFile
    )

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
import "purty" Purty
    ( Args(Args, Defaults, argsFilePath)
    , Error
    , Purty
    , argsInfo
    , errors
    , handle
    , parseConfig
    , purty
    , run
    )

main :: IO ()
main = do
  cliArgs <- execParser argsInfo
  config@Config{ verbosity } <- parseConfig cliArgs
  let prettyPrintConfig = defaultPrettyPrintConfig
  logOptions <- logOptionsHandle stderr (verbosity == Verbose)
  withLogFunc logOptions $ \logFunc -> do
    let env = Env { config, logFunc, prettyPrintConfig }
    run env (program cliArgs `handle` errors)

program :: Args -> Purty Env Error ()
program = \case
  Args { argsFilePath } -> do
    env <- view envL
    output <- view outputL
    logDebug ("Env: " <> display env)
    logDebug ("Converting " <> display argsFilePath <> " to an absolute path")
    absPath <- absolutize argsFilePath
    logDebug ("Converted file to absolute: " <> displayShow absPath)
    logDebug "Running main `purty` program"
    stream <- purty absPath
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
