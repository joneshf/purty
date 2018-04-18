module Main where

import "rio" RIO
import qualified "directory" System.Directory as IO
import qualified "base" System.IO as IO

import "prettyprinter" Data.Text.Prettyprint.Doc
    ( defaultLayoutOptions
    )
import "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text (renderIO)
import "optparse-applicative" Options.Applicative            (execParser)

import "purty" Purty
    ( Args(..)
    , Env(Env)
    , PrettyPrintConfig(PrettyPrintConfig)
    , argsInfo
    , envArgs
    , envLogFunc
    , envPrettyPrintConfig
    , layoutOptions
    , makePathDumb
    , purty
    )

main :: IO ()
main = do
  envArgs@Args{ verbose, filePath, inPlace } <- execParser argsInfo
  let envPrettyPrintConfig =
        PrettyPrintConfig { layoutOptions = defaultLayoutOptions }
  logOptions <- logOptionsHandle stderr verbose
  withLogFunc logOptions $ \envLogFunc -> do
    let env = Env { envArgs, envLogFunc, envPrettyPrintConfig }
    runRIO env $ do
      logDebug ("Env: " <> display env)
      logDebug "Running main `purty` program"
      stream' <- purty
      case stream' of
        Left err -> do
          logError "Problem parsing module"
          logError (displayShow err)
        Right stream -> do
          logDebug "Successfully created stream for rendering"
          logDebug (displayShow $ void stream)
          case inPlace of
            True -> liftIO $ do
              let dumbPath = makePathDumb filePath
              tmp <- IO.getTemporaryDirectory
              (fp, h) <- IO.openTempFile tmp "purty.purs"
              renderIO h stream
              hFlush h
              hClose h
              IO.copyPermissions dumbPath fp
              IO.copyFile fp dumbPath
              IO.removeFile fp
            False -> do
              logDebug "Printing to stdout"
              liftIO $ renderIO stdout stream