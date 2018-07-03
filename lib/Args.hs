module Args where

import "rio" RIO

import "dhall" Dhall                              (auto, input)
import "optparse-applicative" Options.Applicative
    ( Parser
    , ParserInfo
    , argument
    , flag
    , flag'
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
import "path" Path                                (parseAbsFile, parseRelFile)

import "this" Env
    ( Config(Config)
    , Formatting(Dynamic, Static)
    , Output(InPlace, StdOut)
    , PurtyFilePath(AbsFile, RelFile, Unparsed)
    , Verbosity(NotVerbose, Verbose)
    , defaultConfig
    )

import qualified "rio" RIO.Text.Lazy

import qualified "this" Env

data Args
  = Args
    { filePath   :: !PurtyFilePath
    , formatting :: !Formatting
    , output     :: !Output
    , verbosity  :: !Verbosity
    }
  | Defaults
  deriving (Generic)

instance Display Args where
  display = \case
    Args { filePath, formatting, verbosity, output } ->
      "{"
        <> display filePath
        <> ", "
        <> display formatting
        <> ", "
        <> display output
        <> ", "
        <> display verbosity
        <> "}"
    Defaults -> "Defaults"

args :: Parser Args
args =
  parserDefaults
    <|> Args
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

parseConfig :: (MonadUnliftIO f) => Args -> f Config
parseConfig = \case
  Args { formatting, output, verbosity } -> do
    result <- tryIO (readFileUtf8 "./.purty.dhall")
    case result of
      Left _ ->
        pure Config
          { Env.formatting = formatting
          , Env.output = output
          , Env.verbosity = verbosity
          }
      Right contents -> do
        config <- liftIO (input auto (RIO.Text.Lazy.fromStrict contents))
        pure Config
          { Env.formatting = case formatting of
              Static  -> Env.formatting config
              Dynamic -> Dynamic
          , Env.output = case output of
              StdOut  -> Env.output config
              InPlace -> InPlace
          , Env.verbosity = case verbosity of
              NotVerbose -> Env.verbosity config
              Verbose    -> Verbose
          }
  Defaults -> pure defaultConfig

parserDefaults :: Parser Args
parserDefaults = flag' Defaults meta
  where
  meta =
    help
      ( "Display default values for configuration."
      <> " You can save this to `.purty.dhall` as a starting point"
      )
      <> long "defaults"

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

parserFormatting :: Parser Formatting
parserFormatting = flag Static Dynamic meta
  where
  meta =
    help "Pretty print taking line length into account"
      <> long "dynamic"

parserOutput :: Parser Output
parserOutput = flag StdOut InPlace meta
  where
  meta =
    help "Format file in-place"
      <> long "write"

parserVerbosity :: Parser Verbosity
parserVerbosity = flag NotVerbose Verbose meta
  where
  meta =
    help "Print debugging information to STDERR while running"
      <> long "verbose"
