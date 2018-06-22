module Purty where

import "rio" RIO

import "lens" Control.Lens                        (Prism', prism)
import "lens" Control.Monad.Error.Lens            (throwing)
import "mtl" Control.Monad.Except
    ( ExceptT
    , MonadError
    , runExceptT
    )
import "transformers" Control.Monad.Trans.Except  (catchE)
import "prettyprinter" Data.Text.Prettyprint.Doc  (SimpleDocStream, layoutSmart)
import "dhall" Dhall                              (auto, input)
import "purescript" Language.PureScript           (parseModuleFromFile)
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
import "path" Path
    ( Abs
    , File
    , Path
    , fromAbsFile
    , parseAbsFile
    , parseRelFile
    )
import "base" System.Exit                         (exitFailure)
import "parsec" Text.Parsec                       (ParseError)

import "this" Env
    ( Config(Config)
    , Env
    , Formatting(Dynamic, Static)
    , HasFormatting(formattingL)
    , HasLayoutOptions(layoutOptionsL)
    , Output(InPlace, StdOut)
    , PurtyFilePath(AbsFile, RelFile, Unparsed)
    , Verbosity(NotVerbose, Verbose)
    , defaultConfig
    , formatting
    , output
    , verbosity
    )

import qualified "rio" RIO.Text.Lazy

import qualified "this" Purty.AST
import qualified "this" Purty.Doc.Dynamic
import qualified "this" Purty.Doc.Static

purty ::
  ( HasFormatting env
  , HasLayoutOptions env
  , HasLogFunc env
  , Purty.AST.IsMissingName error
  , IsParseError error
  ) =>
  Path Abs File ->
  Purty env error (SimpleDocStream a)
purty filePath = do
  formatting <- view formattingL
  layoutOptions <- view layoutOptionsL
  contents <- readFileUtf8 (fromAbsFile filePath)
  logDebug "Read file contents:"
  logDebug (display contents)
  (_, m) <- either (throwing _ParseError) pure (parseModuleFromFile id (fromAbsFile filePath, contents))
  logDebug "Parsed module:"
  logDebug (displayShow m)
  ast <- Purty.AST.fromPureScript m
  logDebug "Converted AST:"
  logDebug (display ast)
  case formatting of
    Dynamic -> pure (layoutSmart layoutOptions $ Purty.Doc.Dynamic.fromModule ast)
    Static  -> pure (layoutSmart layoutOptions $ Purty.Doc.Static.fromModule ast)

newtype Purty env error c
  = Purty (ReaderT env (ExceptT error IO) c)
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadError error
    , MonadIO
    , MonadReader env
    )

handle :: Purty a b c -> (b -> Purty a d c) -> Purty a d c
handle (Purty (ReaderT f)) g = Purty $ ReaderT $ \env ->
  catchE (f env) $ \y -> case g y of
    Purty h -> runReaderT h env

run :: a -> Purty a Void b -> IO b
run env (Purty f) = do
  result <- runExceptT (runReaderT f env)
  either absurd pure result

data Error
  = AST Purty.AST.Error
  | Parse ParseError

instance Purty.AST.IsMissingName Error where
  _MissingName = Purty.AST._Error.Purty.AST._MissingName

instance Purty.AST.IsError Error where
  _Error = prism AST $ \case
    AST x -> Right x
    x -> Left x

class (Purty.AST.IsError error, IsParseError error) => IsError error where
  _Error :: Prism' error Error

instance IsError Error where
  _Error = prism id Right

class IsParseError error where
  _ParseError :: Prism' error ParseError

instance IsParseError ParseError where
  _ParseError = prism id Right

instance IsParseError Error where
  _ParseError = prism Parse $ \case
    Parse x -> Right x
    x -> Left x

errors :: Error -> Purty Env error a
errors = \case
  AST err -> do
    logError "Problem converting to our AST"
    logError (display err)
    liftIO exitFailure
  Parse err -> do
    logError "Problem parsing module"
    logError (displayShow err)
    liftIO exitFailure

data Args
  = Args
    { argsFilePath   :: !PurtyFilePath
    , argsFormatting :: !Formatting
    , argsOutput     :: !Output
    , argsVerbosity  :: !Verbosity
    }
  | Defaults
  deriving (Generic)

instance Display Args where
  display = \case
    Args { argsFilePath, argsFormatting, argsVerbosity, argsOutput } ->
      "{"
        <> display argsFilePath
        <> ", "
        <> display argsFormatting
        <> ", "
        <> display argsOutput
        <> ", "
        <> display argsVerbosity
        <> "}"
    Defaults -> "Defaults"

parseConfig :: (MonadUnliftIO f) => Args -> f Config
parseConfig = \case
  Args { argsFormatting, argsOutput, argsVerbosity } -> do
    result <- tryIO (readFileUtf8 "./.purty.dhall")
    case result of
      Left _ ->
        pure Config
          { formatting = argsFormatting
          , output = argsOutput
          , verbosity = argsVerbosity
          }
      Right contents -> do
        config <- liftIO (input auto (RIO.Text.Lazy.fromStrict contents))
        pure Config
          { formatting = case argsFormatting of
              Static  -> formatting config
              Dynamic -> Dynamic
          , output = case argsOutput of
              StdOut  -> output config
              InPlace -> InPlace
          , verbosity = case argsVerbosity of
              NotVerbose -> verbosity config
              Verbose    -> Verbose
          }
  Defaults -> pure defaultConfig

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

parserDefaults :: Parser Args
parserDefaults = flag' Defaults meta
  where
  meta =
    help
      ( "Display default values for configuration."
      <> " You can save this to `.purty.dhall` as a starting point"
      )
      <> long "defaults"

parserFormatting :: Parser Formatting
parserFormatting = flag Static Dynamic meta
  where
  meta =
    help "Pretty print taking line length into account"
      <> long "dynamic"

parserVerbosity :: Parser Verbosity
parserVerbosity = flag NotVerbose Verbose meta
  where
  meta =
    help "Print debugging information to STDERR while running"
      <> long "verbose"

parserOutput :: Parser Output
parserOutput = flag StdOut InPlace meta
  where
  meta =
    help "Format file in-place"
      <> long "write"

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
