module Main where

import "rio" RIO hiding (log)

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
  run args (Log.handle config) \log -> do
    result <- Purty.run log args
    case result of
      Just _err -> exitFailure
      Nothing -> exitSuccess

run ::
  Args.Args ->
  Control.Monad.Component.ComponentM a ->
  (a -> IO ()) ->
  IO ()
run args component f
  | Args.debug args =
    Control.Monad.Component.runComponentM1
      (runSimpleApp . logInfo . display)
      "purty"
      component
      f
  | otherwise = Control.Monad.Component.runComponentM "purty" component f
