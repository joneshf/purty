module Purty where

import "rio" RIO

import "prettyprinter" Data.Text.Prettyprint.Doc
    ( LayoutOptions
    , SimpleDocStream
    , defaultLayoutOptions
    , layoutSmart
    )
import "purescript" Language.PureScript           (parseModuleFromFile)
import "optparse-applicative" Options.Applicative
    ( Parser
    , ParserInfo
    , argument
    , fullDesc
    , header
    , help
    , helper
    , info
    , long
    , maybeReader
    , metavar
    , progDesc
    , switch
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

purty :: (HasArgs env, HasPrettyPrintConfig env) => RIO env (Either ParseError (SimpleDocStream a))
purty = do
  Args { filePath } <- view argsL
  PrettyPrintConfig { layoutOptions } <- view prettyPrintConfigL
  absFilePath <- either pure makeAbsolute filePath
  contents <- readFileUtf8 (fromAbsFile absFilePath)
  pure $ do
    (_, m) <- parseModuleFromFile id (fromAbsFile absFilePath, contents)
    pure (layoutSmart layoutOptions $ Doc.fromModule m)

data Args
  = Args
    { filePath :: !(Either (Path Abs File) (Path Rel File))
    , verbose  :: !Bool
    }

class HasArgs env where
  argsL :: Lens' env Args

args :: Parser Args
args =
  Args
    <$> argument
      ( fmap Left (maybeReader parseAbsFile)
      <|> fmap Right (maybeReader parseRelFile)
      )
      ( help "PureScript file to pretty print"
      <> metavar "FILE"
      )
    <*> switch
      ( help "Print debugging information to STDERR while running"
      <> long "verbose"
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
    , envLogFunc           :: !LogFunc
    , envPrettyPrintConfig :: !PrettyPrintConfig
    }

defaultEnv :: LogFunc -> Path Abs File -> Env
defaultEnv envLogFunc filePath =
  Env { envArgs, envLogFunc, envPrettyPrintConfig }
    where
    envArgs = Args { verbose = True, filePath = Left filePath }
    envPrettyPrintConfig =
      PrettyPrintConfig { layoutOptions = defaultLayoutOptions }

class HasEnv env where
  envL :: Lens' env Env

instance HasArgs Env where
  argsL f env = (\envArgs -> env { envArgs }) <$> f (envArgs env)

instance HasEnv Env where
  envL = id

instance HasLogFunc Env where
  logFuncL f env = (\envLogFunc -> env { envLogFunc }) <$> f (envLogFunc env)

instance HasPrettyPrintConfig Env where
  prettyPrintConfigL f env = (\envPrettyPrintConfig -> env { envPrettyPrintConfig }) <$> f (envPrettyPrintConfig env)
