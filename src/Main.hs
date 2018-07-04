module Main where

import "rio" RIO

import "freer-simple" Control.Monad.Freer
    ( interpretM
    , reinterpret
    , runM
    )
import "freer-simple" Control.Monad.Freer.Error   (Error(Error))
import "freer-simple" Control.Monad.Freer.Reader  (runReader)
import "optparse-applicative" Options.Applicative (execParser)

import "purty" Args (argsInfo, parseConfig)
import "purty" Env
    ( Config(Config, formatting, output, verbosity)
    , Verbosity(Verbose)
    , defaultLayoutOptions
    )

import qualified "purty" Error
import qualified "purty" Exit
import qualified "purty" File
import qualified "purty" Log
import qualified "purty" Output
import qualified "purty" Purty

main :: IO ()
main = do
  cliArgs <- execParser argsInfo
  Config{ formatting, output, verbosity } <- parseConfig cliArgs
  logOptions <- logOptionsHandle stderr (verbosity == Verbose)
  withLogFunc logOptions $ \logFunc ->
    runM
      $ interpretM Output.io
      $ interpretM (Log.io logFunc)
      $ interpretM File.io
      $ interpretM Exit.io
      $ reinterpret (\(Error e) -> Error.parseError e)
      $ runReader output
      $ runReader defaultLayoutOptions
      $ runReader formatting
      $ Purty.program cliArgs
