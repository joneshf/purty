module Purty where

import "rio" RIO

import "prettyprinter" Data.Text.Prettyprint.Doc
    ( LayoutOptions
    , PageWidth(AvailablePerLine, Unbounded)
    , SimpleDocStream
    , defaultLayoutOptions
    , layoutPageWidth
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

purty ::
  (HasArgs env, HasLogFunc env, HasPrettyPrintConfig env) =>
  RIO env (Either ParseError (SimpleDocStream a))
purty = do
  Args { filePath } <- view argsL
  PrettyPrintConfig { layoutOptions } <- view prettyPrintConfigL
  absFilePath <- either pure makeAbsolute filePath
  logDebug ("Converted file to absolute: " <> displayShow absFilePath)
  contents <- readFileUtf8 (fromAbsFile absFilePath)
  logDebug "Read file contents:"
  logDebug (display contents)
  pure $ do
    (_, m) <- parseModuleFromFile id (fromAbsFile absFilePath, contents)
    pure (layoutSmart layoutOptions $ Doc.fromModule m)

data Args
  = Args
    { filePath :: !(Either (Path Abs File) (Path Rel File))
    , verbose  :: !Bool
    }

instance Display Args where
  display Args { filePath, verbose } =
    "{"  <> displayFilePath filePath <> ", " <> displayVerbose verbose <> "}"
      where
      displayFilePath = \case
        Left absFile -> "Absolute file: " <> displayShow absFile
        Right relFile -> "Relative file: " <> displayShow relFile
      displayVerbose = \case
        True -> "Verbose"
        False -> "Not verbose"

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

instance Display PrettyPrintConfig where
  display PrettyPrintConfig { layoutOptions } =
    case layoutPageWidth layoutOptions of
      AvailablePerLine width ribbon ->
        "{Page width: "
          <> display width
          <> ", Ribbon width: "
          <> display (truncate (ribbon * fromIntegral width) :: Int)
          <> "}"
      Unbounded -> "Unbounded"

class HasPrettyPrintConfig env where
  prettyPrintConfigL :: Lens' env PrettyPrintConfig

data Env
  = Env
    { envArgs              :: !Args
    , envLogFunc           :: !LogFunc
    , envPrettyPrintConfig :: !PrettyPrintConfig
    }

instance Display Env where
  display Env { envArgs, envPrettyPrintConfig } =
    "{Args: "
      <> display envArgs
      <> ", PrettyPrintConfig: "
      <> display envPrettyPrintConfig
      <> "}"

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
