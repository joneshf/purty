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
    , Dir
    , File
    , Path
    , Rel
    , fileExtension
    , fromAbsFile
    , parseAbsDir
    , parseAbsFile
    , parseRelDir
    , parseRelFile
    )
import "path-io" Path.IO
    ( makeAbsolute
    , resolveDir'
    , resolveFile'
    , walkDirAccum
    )
import "parsec" Text.Parsec                       (ParseError)

import qualified "this" Doc

purty ::
  (HasLogFunc env, HasPrettyPrintConfig env) =>
  Path Abs File ->
  RIO env (Either ParseError (SimpleDocStream a))
purty absPath = do
  PrettyPrintConfig { layoutOptions } <- view prettyPrintConfigL
  contents <- readFileUtf8 (fromAbsFile absPath)
  logDebug "Read file contents:"
  logDebug (display contents)
  case parseModuleFromFile id (fromAbsFile absPath, contents) of
    Left e -> do
      logDebug "Parsing failed:"
      logDebug (displayShow e)
      pure (Left e)
    Right (_, m) -> do
      logDebug "Parsed module:"
      logDebug (displayShow m)
      pure (Right $ layoutSmart layoutOptions $ Doc.fromModule m)

data PurtyFilePath
  = AbsFile (Path Abs File)
  | RelFile (Path Rel File)
  | AbsDir (Path Abs Dir)
  | RelDir (Path Rel Dir)
  | Unparsed String

absolutize :: (MonadIO m, Alternative m) => PurtyFilePath -> m [Path Abs File]
absolutize fp = case fp of
  AbsFile absolute -> pure [absolute]
  AbsDir absolute  -> collectPSFiles absolute
  RelDir relative  -> collectPSFiles relative
  Unparsed path    -> resolveUnparsed path
  RelFile relative -> pure <$> makeAbsolute relative
  where
    isPurs file = fileExtension file == ".purs"
    collectPSFiles = walkDirAccum Nothing (\_ _ -> pure . filter isPurs)
    resolveUnparsed path =
      (pure <$> resolveFile' path)
      <|> (resolveDir' path >>= collectPSFiles)

data Args
  = Args
    { filePath :: !PurtyFilePath
    , verbose  :: !Bool
    , inPlace  :: !Bool
    }

instance Display Args where
  display Args { filePath, verbose, inPlace } =
    "{"  <> displayFilePath filePath <> ", " <> displayVerbose verbose <> ", " <> displayInPlace inPlace <> "}"
      where
      displayFilePath = \case
        AbsFile absFile -> "Absolute file: " <> displayShow absFile
        RelFile relFile -> "Relative file: " <> displayShow relFile
        AbsDir absDir -> "Absolute directory: " <> displayShow absDir
        RelDir relDir -> "Relative directory: " <> displayShow relDir
        Unparsed maybePath -> "I dunno, man: " <> displayShow maybePath
      displayVerbose = \case
        True -> "Verbose"
        False -> "Not verbose"
      displayInPlace = \case
        True -> "Formatting files in-place"
        False -> "Writing formatted files to stdout"

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
      <|> fmap AbsDir (maybeReader parseAbsDir)
      <|> fmap RelDir (maybeReader parseRelDir)
      <|> fmap Unparsed (maybeReader Just)

parserVerbose :: Parser Bool
parserVerbose = switch meta
  where
  meta =
    help "Print debugging information to STDERR while running"
      <> long "verbose"

parserInPlace :: Parser Bool
parserInPlace = switch meta
  where
  meta =
    help "Format file in-place"
      <> long "write"

args :: Parser Args
args =
  Args
    <$> parserFilePath
    <*> parserVerbose
    <*> parserInPlace

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
    envArgs = Args { verbose = True, inPlace = False, filePath = AbsFile filePath }
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
