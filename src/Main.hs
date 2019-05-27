module Main where

import "rio" RIO hiding (log)

import qualified "purty" Error
import qualified "purty" Args
import qualified "optparse-applicative" Options.Applicative
import qualified "purty" Log
import qualified "purty" Purty
import qualified "componentm" Control.Monad.Component

main :: IO ()
main = do
  args <- Options.Applicative.execParser Args.info
  let config =
        Log.Config
          { Log.name = "Log"
          , Log.verbose = Args.debug args
          }
  code <- run args (Log.handle config) \log -> do
    result <- Purty.run log args
    case result of
      Just err -> do
        Log.info log (Error.unwrap err)
        pure (ExitFailure 1)
      Nothing -> pure ExitSuccess
  exitWith code

run ::
  Args.Args ->
  Control.Monad.Component.ComponentM a ->
  (a -> IO ExitCode) ->
  IO ExitCode
run args component f
  | Args.debug args =
    Control.Monad.Component.runComponentM1
      (runSimpleApp . logInfo . display)
      "purty"
      component
      f
  | otherwise = Control.Monad.Component.runComponentM "purty" component f
