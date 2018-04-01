module Main where

import "protolude" Protolude

import "prettyprinter" Data.Text.Prettyprint.Doc
    ( LayoutOptions
    , defaultLayoutOptions
    , layoutSmart
    )
import "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text (renderIO)
import "purescript" Language.PureScript
    ( parseModuleFromFile
    )
import "microlens-platform" Lens.Micro.Platform              (Lens', view)
import "optparse-applicative" Options.Applicative
    ( Parser
    , ParserInfo
    , argument
    , execParser
    , fullDesc
    , header
    , help
    , helper
    , info
    , maybeReader
    , metavar
    , progDesc
    )
import "path" Path
    ( Abs
    , File
    , Path
    , fromAbsFile
    , parseAbsFile
    )

import qualified "this" Doc

main :: IO ()
main = do
  envArgs <- execParser argsInfo
  let envPrettyPrintConfig =
        PrettyPrintConfig { layoutOptions = defaultLayoutOptions }
  runPurty Env { envArgs, envPrettyPrintConfig } purty

purty :: (HasArgs env, HasPrettyPrintConfig env) => Purty env ()
purty = do
  Args { filePath } <- view argsL
  PrettyPrintConfig { layoutOptions } <- view prettyPrintConfigL
  contents <- liftIO $ readFile (fromAbsFile filePath)
  case parseModuleFromFile identity (fromAbsFile filePath, contents) of
    Left error -> do
      putErrText "Problem parsing module"
      putErrText (show error)
    Right (file, m) -> do
      putText $ "parsed " <> toS file
      liftIO $ renderIO stdout $ layoutSmart layoutOptions (Doc.fromModule m)

data Args
  = Args
    { filePath :: !(Path Abs File)
    }

class HasArgs env where
  argsL :: Lens' env Args

args :: Parser Args
args =
  Args
    <$> argument
      (maybeReader parseAbsFile)
      ( help "PureScript file to pretty print"
      <> metavar "FILE"
      )

argsInfo :: ParserInfo Args
argsInfo =
  info
    (helper <*> args)
    ( fullDesc
    <> progDesc "Pretty print a PureScript file"
    <> header "purty - A PureScript pretty-printer"
    )

data PrettyPrintConfig
  = PrettyPrintConfig
    { layoutOptions :: !LayoutOptions
    }

class HasPrettyPrintConfig env where
  prettyPrintConfigL :: Lens' env PrettyPrintConfig

data Env
  = Env
    { envArgs              :: !Args
    , envPrettyPrintConfig :: !PrettyPrintConfig
    }

class HasEnv env where
  envL :: Lens' env Env

instance HasArgs Env where
  argsL f env = (\envArgs -> env { envArgs }) <$> f (envArgs env)

instance HasEnv Env where
  envL = identity

instance HasPrettyPrintConfig Env where
  prettyPrintConfigL f env = (\envPrettyPrintConfig -> env { envPrettyPrintConfig }) <$> f (envPrettyPrintConfig env)

-- Locally defined rio since dependencies are wild.
newtype Purty r a
  = Purty { unPurty :: ReaderT r IO a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader r
    )

runPurty :: r -> Purty r a -> IO a
runPurty r (Purty x) = runReaderT x r
