module Purty where

import "rio" RIO

import "prettyprinter" Data.Text.Prettyprint.Doc
    ( LayoutOptions(LayoutOptions, layoutPageWidth)
    , PageWidth(AvailablePerLine, Unbounded)
    , SimpleDocStream
    , layoutPageWidth
    , layoutSmart
    )
import "dhall" Dhall                              (Interpret, auto, input)
import "purescript" Language.PureScript           (parseModuleFromFile)
import "optparse-applicative" Options.Applicative
    ( Parser
    , ParserInfo
    , argument
    , flag
    , fullDesc
    , header
    , help
    , helper
    , info
    , long
    , maybeReader
    , metavar
    , progDesc
    )
import "optparse-text" Options.Applicative.Text   (text)
import "path" Path
    ( Abs
    , File
    , Path
    , Rel
    , fromAbsFile
    , fromRelFile
    , parseAbsFile
    , parseRelFile
    )
import "path-io" Path.IO                          (makeAbsolute, resolveFile')
import "rio" RIO.Text                             (unpack)
import "parsec" Text.Parsec                       (ParseError)

import qualified "rio" RIO.Text.Lazy

import qualified "this" Doc.Dynamic
import qualified "this" Doc.Static

purty ::
  (HasConfig env, HasLogFunc env, HasPrettyPrintConfig env) =>
  Path Abs File ->
  RIO env (Either ParseError (SimpleDocStream a))
purty filePath = do
  Config { formatting } <- view configL
  PrettyPrintConfig { layoutOptions } <- view prettyPrintConfigL
  contents <- readFileUtf8 (fromAbsFile filePath)
  logDebug "Read file contents:"
  logDebug (display contents)
  case parseModuleFromFile id (fromAbsFile filePath, contents) of
    Left e -> do
      logDebug "Parsing failed:"
      logDebug (displayShow e)
      pure (Left e)
    Right (_, m) -> do
      logDebug "Parsed module:"
      logDebug (displayShow m)
      case formatting of
        Dynamic -> pure (Right $ layoutSmart layoutOptions $ Doc.Dynamic.fromModule m)
        Static  -> pure (Right $ layoutSmart layoutOptions $ Doc.Static.fromModule m)

data PurtyFilePath
  = AbsFile !(Path Abs File)
  | RelFile !(Path Rel File)
  | Unparsed !Text

instance Display PurtyFilePath where
  display = \case
    AbsFile path -> "Absolute file: " <> displayShow (fromAbsFile path)
    RelFile path -> "Relative file: " <> displayShow (fromRelFile path)
    Unparsed path -> "Unparsed: " <> displayShow path

absolutize :: MonadIO m => PurtyFilePath -> m (Path Abs File)
absolutize fp = case fp of
  AbsFile absolute -> pure absolute
  RelFile relative -> makeAbsolute relative
  Unparsed path    -> resolveFile' (unpack path)

data Args
  = Args
    { argsFilePath   :: !PurtyFilePath
    , argsFormatting :: !Formatting
    , argsOutput     :: !Output
    , argsVerbosity  :: !Verbosity
    }
  deriving (Generic)

instance Display Args where
  display Args { argsFilePath, argsFormatting, argsVerbosity, argsOutput } =
    "{"
      <> display argsFilePath
      <> ", "
      <> display argsFormatting
      <> ", "
      <> display argsOutput
      <> ", "
      <> display argsVerbosity
      <> "}"

data Config
  = Config
    { formatting :: !Formatting
    , output     :: !Output
    , verbosity  :: !Verbosity
    }
  deriving (Generic)

instance Display Config where
  display Config { formatting, verbosity, output } =
    "{"
      <> display formatting
      <> ", "
      <> display output
      <> ", "
      <> display verbosity
      <> "}"

instance Interpret Config

class HasConfig env where
  configL :: Lens' env Config

parseConfig :: (MonadUnliftIO f) => Args -> f Config
parseConfig cliArgs = do
  result <- tryIO (readFileUtf8 "./.purty.dhall")
  case result of
    Left _ ->
      pure Config
        { formatting = argsFormatting cliArgs
        , output = argsOutput cliArgs
        , verbosity = argsVerbosity cliArgs
        }
    Right contents -> do
      config <- liftIO (input auto (RIO.Text.Lazy.fromStrict contents))
      pure Config
        { formatting = case argsFormatting cliArgs of
            Static  -> formatting config
            Dynamic -> Dynamic
        , output = case argsOutput cliArgs of
            StdOut  -> output config
            InPlace -> InPlace
        , verbosity = case argsVerbosity cliArgs of
            NotVerbose -> verbosity config
            Verbose    -> Verbose
        }

parserFilePath :: Parser PurtyFilePath
parserFilePath = argument parser meta
  where
  meta =
    help "PureScript file to pretty print"
      <> metavar "FILE"
  parser =
    fmap AbsFile (maybeReader parseAbsFile)
      <|> fmap RelFile (maybeReader parseRelFile)
      <|> fmap Unparsed text

-- |
-- How we want to pretty print
--
-- Dynamic formatting takes line length into account.
-- Static formatting is always the same.
data Formatting
  = Dynamic
  | Static
  deriving (Generic)

instance Display Formatting where
  display = \case
    Dynamic -> "Dynamic"
    Static -> "Static"

instance Interpret Formatting

parserFormatting :: Parser Formatting
parserFormatting = flag Static Dynamic meta
  where
  meta =
    help "Pretty print taking line length into account"
      <> long "dynamic"

-- |
-- The minimum level of logs to display.
--
-- 'Verbose' will display debug logs.
-- Debug logs are pretty noisy, but useful when diagnosing problems.
data Verbosity
  = Verbose
  | NotVerbose
  deriving (Eq, Generic)

instance Display Verbosity where
  display = \case
    Verbose -> "Verbose"
    NotVerbose -> "Not verbose"

instance Interpret Verbosity

parserVerbosity :: Parser Verbosity
parserVerbosity = flag NotVerbose Verbose meta
  where
  meta =
    help "Print debugging information to STDERR while running"
      <> long "verbose"

-- |
-- What to do with the pretty printed output
data Output
  = InPlace
  | StdOut
  deriving (Generic)

instance Display Output where
  display = \case
    InPlace -> "Formatting files in-place"
    StdOut -> "Writing formatted files to stdout"

instance Interpret Output

parserOutput :: Parser Output
parserOutput = flag StdOut InPlace meta
  where
  meta =
    help "Format file in-place"
      <> long "write"

args :: Parser Args
args =
  Args
    <$> parserFilePath
    <*> parserFormatting
    <*> parserOutput
    <*> parserVerbosity

argsInfo :: ParserInfo Args
argsInfo =
  info
    (helper <*> args)
    ( fullDesc
    <> progDesc "Pretty print a PureScript file"
    <> header "purty - A PureScript pretty-printer"
    )

newtype PrettyPrintConfig
  = PrettyPrintConfig
    { layoutOptions :: LayoutOptions
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

defaultPrettyPrintConfig :: PrettyPrintConfig
defaultPrettyPrintConfig =
  PrettyPrintConfig
    { layoutOptions =
      LayoutOptions
      { layoutPageWidth =
        AvailablePerLine 80 1
      }
    }

data Env
  = Env
    { envConfig            :: !Config
    , envLogFunc           :: !LogFunc
    , envPrettyPrintConfig :: !PrettyPrintConfig
    }

instance Display Env where
  display Env { envConfig, envPrettyPrintConfig } =
    "{Config: "
      <> display envConfig
      <> ", PrettyPrintConfig: "
      <> display envPrettyPrintConfig
      <> "}"

defaultEnv :: Formatting -> LogFunc -> Env
defaultEnv formatting envLogFunc =
  Env { envConfig, envLogFunc, envPrettyPrintConfig }
    where
    envConfig = Config { formatting, output, verbosity }
    envPrettyPrintConfig = defaultPrettyPrintConfig
    output = StdOut
    verbosity = Verbose

class HasEnv env where
  envL :: Lens' env Env

instance HasConfig Env where
  configL f env = (\envConfig -> env { envConfig }) <$> f (envConfig env)

instance HasEnv Env where
  envL = id

instance HasLogFunc Env where
  logFuncL f env = (\envLogFunc -> env { envLogFunc }) <$> f (envLogFunc env)

instance HasPrettyPrintConfig Env where
  prettyPrintConfigL f env = (\envPrettyPrintConfig -> env { envPrettyPrintConfig }) <$> f (envPrettyPrintConfig env)
