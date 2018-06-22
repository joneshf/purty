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
import "base" System.Exit                                    (exitFailure)
import "parsec" Text.Parsec                                  (ParseError)

import "purty" Purty
    ( Args(Args, Defaults, argsFilePath)
    , Config(Config, verbosity)
    , Env(Env)
    , Output(InPlace, StdOut)
    , Purty
    , Verbosity(Verbose)
    , absolutize
    , argsInfo
    , defaultConfig
    , defaultPrettyPrintConfig
    , envConfig
    , envL
    , envLogFunc
    , envPrettyPrintConfig
    , handle
    , outputL
    , parseConfig
    , purty
    , run
    )

main :: IO ()
main = do
  cliArgs <- execParser argsInfo
  envConfig@Config{ verbosity } <- parseConfig cliArgs
  let envPrettyPrintConfig = defaultPrettyPrintConfig
  logOptions <- logOptionsHandle stderr (verbosity == Verbose)
  withLogFunc logOptions $ \envLogFunc -> do
    let env = Env { envConfig, envLogFunc, envPrettyPrintConfig }
    run env (program cliArgs `handle` parseError)

parseError :: ParseError -> Purty Env error a
parseError err = do
  logError "Problem parsing module"
  logError (displayShow err)
  liftIO exitFailure

program :: Args -> Purty Env ParseError ()
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
