module Purty where

import "rio" RIO

import "prettyprinter" Data.Text.Prettyprint.Doc
    ( LayoutOptions(LayoutOptions, layoutPageWidth)
    , PageWidth(AvailablePerLine, Unbounded)
    , SimpleDocStream
    , layoutPageWidth
    , layoutSmart
    )
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
import "path" Path
    ( Abs
    , File
    , Path
    , Rel
    , fromAbsFile
    , parseAbsFile
    , parseRelFile
    )
import "path-io" Path.IO                          (makeAbsolute, resolveFile')
import "parsec" Text.Parsec                       (ParseError)

import qualified "this" Doc.Dynamic
import qualified "this" Doc.Static

purty ::
  (HasArgs env, HasLogFunc env, HasPrettyPrintConfig env) =>
  PurtyFilePath ->
  RIO env (Either ParseError (SimpleDocStream a))
purty filePath = do
  Args { formatting } <- view argsL
  PrettyPrintConfig { layoutOptions } <- view prettyPrintConfigL
  absFilePath <- absolutize filePath
  logDebug ("Converted file to absolute: " <> displayShow absFilePath)
  contents <- readFileUtf8 (fromAbsFile absFilePath)
  logDebug "Read file contents:"
  logDebug (display contents)
  case parseModuleFromFile id (fromAbsFile absFilePath, contents) of
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
  = AbsFile (Path Abs File)
  | RelFile (Path Rel File)
  | Unparsed String

absolutize :: MonadIO m => PurtyFilePath -> m (Path Abs File)
absolutize fp = case fp of
  AbsFile absolute -> pure absolute
  RelFile relative -> makeAbsolute relative
  Unparsed path    -> resolveFile' path

data Args
  = Args
    { formatting :: !Formatting
    , output     :: !Output
    , verbosity  :: !Verbosity
    }

instance Display Args where
  display Args { formatting, verbosity, output } =
    "{"
      <> displayFormatting formatting
      <> ", "
      <> displayOutput output
      <> ", "
      <> displayVerbosity verbosity
      <> "}"
      where
      displayFormatting = \case
        Dynamic -> "Dynamic"
        Static -> "Static"
      displayVerbosity = \case
        Verbose -> "Verbose"
        NotVerbose -> "Not verbose"
      displayOutput = \case
        InPlace -> "Formatting files in-place"
        StdOut -> "Writing formatted files to stdout"

class HasArgs env where
  argsL :: Lens' env Args

parserFilePath :: Parser PurtyFilePath
parserFilePath = argument parser meta
  where
  meta =
    help "PureScript file to pretty print"
      <> metavar "FILE"
  parser =
    fmap AbsFile (maybeReader parseAbsFile)
      <|> fmap RelFile (maybeReader parseRelFile)
      <|> fmap Unparsed (maybeReader Just)

-- |
-- How we want to pretty print
--
-- Dynamic formatting takes line length into account.
-- Static formatting is always the same.
data Formatting
  = Dynamic
  | Static

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
  deriving (Eq)

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

parserOutput :: Parser Output
parserOutput = flag StdOut InPlace meta
  where
  meta =
    help "Format file in-place"
      <> long "write"

args :: Parser Args
args =
  Args
    <$> parserFormatting
    <*> parserOutput
    <*> parserVerbosity

argsInfo :: ParserInfo (Args, PurtyFilePath)
argsInfo =
  info
    (helper <*> ((,) <$> args <*> parserFilePath))
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

defaultEnv :: Formatting -> LogFunc -> Env
defaultEnv formatting envLogFunc =
  Env { envArgs, envLogFunc, envPrettyPrintConfig }
    where
    envArgs = Args { formatting, output, verbosity }
    envPrettyPrintConfig = defaultPrettyPrintConfig
    output = StdOut
    verbosity = Verbose

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
