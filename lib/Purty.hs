module Purty where

import "protolude" Protolude

import "prettyprinter" Data.Text.Prettyprint.Doc
    ( LayoutOptions
    , SimpleDocStream
    , defaultLayoutOptions
    , layoutSmart
    )
import "purescript" Language.PureScript           (parseModuleFromFile)
import "microlens-platform" Lens.Micro.Platform   (Lens', view)
import "optparse-applicative" Options.Applicative
    ( Parser
    , ParserInfo
    , argument
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
    , Rel
    , fromAbsFile
    , parseAbsFile
    , parseRelFile
    )
import "path-io" Path.IO                          (makeAbsolute)
import "parsec" Text.Parsec                       (ParseError)

import qualified "this" Doc

purty :: (HasArgs env, HasPrettyPrintConfig env) => Purty env (Either ParseError (SimpleDocStream a))
purty = do
  Args { filePath } <- view argsL
  PrettyPrintConfig { layoutOptions } <- view prettyPrintConfigL
  absFilePath <- either pure makeAbsolute filePath
  contents <- liftIO $ readFile (fromAbsFile absFilePath)
  pure $ do
    (_, m) <- parseModuleFromFile identity (fromAbsFile absFilePath, contents)
    pure (layoutSmart layoutOptions $ Doc.fromModule m)

data Args
  = Args
    { filePath :: !(Either (Path Abs File) (Path Rel File))
    }

class HasArgs env where
  argsL :: Lens' env Args

args :: Parser Args
args =
  Args
    <$> argument
      ( map Left (maybeReader parseAbsFile)
      <|> map Right (maybeReader parseRelFile)
      )
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

defaultEnv :: Path Abs File -> Env
defaultEnv filePath = Env { envArgs, envPrettyPrintConfig }
  where
  envArgs = Args { filePath = Left filePath }
  envPrettyPrintConfig =
    PrettyPrintConfig { layoutOptions = defaultLayoutOptions }

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
