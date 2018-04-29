module Main where

import "rio" RIO hiding (withSystemTempFile)

import "prettyprinter" Data.Text.Prettyprint.Doc
    ( defaultLayoutOptions
    )
import "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text (renderIO)
import "optparse-applicative" Options.Applicative            (execParser)
import "path-io" Path.IO
    ( copyFile
    , copyPermissions
    , withSystemTempFile
    )
import "base" System.Exit                                    (exitFailure)

import "purty" Purty
    ( Args(..)
    , Env(Env)
    , PrettyPrintConfig(PrettyPrintConfig)
    , absolutize
    , argsInfo
    , envArgs
    , envLogFunc
    , envPrettyPrintConfig
    , layoutOptions
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
    files <- absolutize filePath
    runRIO env $ do
      streams <- for files purty
      let zippedPartition tuple (ls, rs) = 
            case tuple of
              (a, Left b) -> ((a, b):ls, rs)
              (a, Right c) ->(ls, (a, c):rs)
          partitionStreams = foldr zippedPartition ([], [])
          (errors, docs) = partitionStreams $ zip files streams
      case errors of
        [] -> for_ docs $ \(path, stream) -> do
          logDebug "Successfully created streams for rendering"
          logDebug (displayShow $ void stream)
          if inPlace then
            liftIO $ withSystemTempFile "purty.purs" $ \fp h -> do
              renderIO h stream
              hClose h
              copyPermissions path fp
              copyFile fp path
          else do
              logDebug "Printing to stdout"
              liftIO $ renderIO stdout stream
        _ -> for_ errors $ \(_, err) -> do
          logError "Problem parsing module"
          logError (displayShow err)
          liftIO exitFailure