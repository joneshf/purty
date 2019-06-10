module Args
  ( Args(..)
  , debug
  , info
  , parse
  , withConfig
  , withInput
  , write
  , writeDefaults
  ) where

import "rio" RIO hiding (log)

import qualified "componentm" Control.Monad.Component
import qualified "bytestring" Data.ByteString.Builder
import qualified "dhall" Dhall
import qualified "dhall" Dhall.Core
import qualified "dhall" Dhall.Map
import qualified "dhall" Dhall.Parser
import qualified "dhall" Dhall.TypeCheck
import qualified "this" Error
import qualified "this" Log
import qualified "optparse-applicative" Options.Applicative
import qualified "rio" RIO.ByteString.Lazy
import qualified "rio" RIO.Directory
import qualified "rio" RIO.File

data Args
  = Defaults Defaults
  | Format Format

data Config
  = Config Output Verbose

instance Display Config where
  display config' = case config' of
    Config output' verbose' ->
      "Config { output = "
        <> display output'
        <> ", verbose = "
        <> display verbose'
        <> " }"

newtype Defaults
  = Defaults' Verbose

data Format
  = Format' Input Output Verbose

instance Display Format where
  display format' = case format' of
    Format' input' output' verbose' ->
      "Format {"
        <> " input = "
        <> display input'
        <> ","
        <> " output = "
        <> display output'
        <> ","
        <> " verbose = "
        <> display verbose'
        <> " }"

data Input
  = InputFile FilePath
  | InputSTDIN

instance Display Input where
  display input' = case input' of
    InputFile file ->
      "InputFile {"
        <> " filePath = "
        <> displayShow file
        <> " }"
    InputSTDIN ->
      "InputSTDIN {"
        <> " }"

data Output
  = STDOUT
  | Write

instance Display Output where
  display output' = case output' of
    STDOUT -> "STDOUT"
    Write  -> "Write"

instance Semigroup Output where
  output1 <> output2 = case (output1, output2) of
    (STDOUT, STDOUT) -> STDOUT
    (STDOUT, Write)  -> Write
    (Write, STDOUT)  -> Write
    (Write, Write)   -> Write

data Verbose
  = NotVerbose
  | Verbose

instance Display Verbose where
  display verbose' = case verbose' of
    NotVerbose -> "NotVerbose"
    Verbose    -> "Verbose"

instance Semigroup Verbose where
  verbose1 <> verbose2 = case (verbose1, verbose2) of
    (NotVerbose, NotVerbose) -> NotVerbose
    (NotVerbose, Verbose)    -> Verbose
    (Verbose, NotVerbose)    -> Verbose
    (Verbose, Verbose)       -> Verbose

args :: Options.Applicative.Parser Args
args =
  asum
    [ fmap Defaults defaults
    , fmap Format format
    ]

config :: Config
config =
  Config STDOUT NotVerbose

configInputType :: Dhall.InputType Config
configInputType = Dhall.InputType { Dhall.declared, Dhall.embed }
  where
  declared :: Dhall.Core.Expr Dhall.Parser.Src Dhall.TypeCheck.X
  declared =
    Dhall.Core.Record
      ( Dhall.Map.fromList
        [ ("output", Dhall.declared outputInputType)
        , ("verbose", Dhall.declared verboseInputType)
        ]
      )

  embed :: Config -> Dhall.Core.Expr Dhall.Parser.Src Dhall.TypeCheck.X
  embed config'' = case config'' of
    Config output' verbose' ->
      Dhall.Core.RecordLit
        ( Dhall.Map.fromList
          [ ("output", Dhall.embed outputInputType output')
          , ("verbose", Dhall.embed verboseInputType verbose')
          ]
        )

configType :: Dhall.Type Config
configType = Dhall.Type { Dhall.expected, Dhall.extract }
  where
  expected :: Dhall.Core.Expr Dhall.Parser.Src Dhall.TypeCheck.X
  expected =
    Dhall.Core.Record
      ( Dhall.Map.fromList
        [ ("output", Dhall.expected outputType)
        , ("verbose", Dhall.expected verboseType)
        ]
      )

  extract :: Dhall.Core.Expr Dhall.Parser.Src Dhall.TypeCheck.X -> Maybe Config
  extract expr = case expr of
    Dhall.Core.RecordLit record -> do
      output'' <- Dhall.Map.lookup "output" record
      output' <- Dhall.extract outputType output''
      verbose'' <- Dhall.Map.lookup "verbose" record
      verbose' <- Dhall.extract verboseType verbose''
      Just (Config output' verbose')
    _ -> Nothing

debug :: Args -> Bool
debug args' = case args' of
  Defaults defaults' -> debugDefaults defaults'
  Format format'     -> debugFormat format'

debugDefaults :: Defaults -> Bool
debugDefaults defaults' = case defaults' of
  Defaults' verbose' -> debugVerbose verbose'

debugFormat :: Format -> Bool
debugFormat format' = case format' of
  Format' _ _ verbose' -> debugVerbose verbose'

debugVerbose :: Verbose -> Bool
debugVerbose verbose' = case verbose' of
  NotVerbose -> False
  Verbose    -> True

defaults :: Options.Applicative.Parser Defaults
defaults =
  Options.Applicative.flag' Defaults' meta
    <*> verbose
  where
  meta :: Options.Applicative.Mod Options.Applicative.FlagFields a
  meta =
    Options.Applicative.help
      ( "Display default values for configuration."
        <> " You can save this to `.purty.dhall` as a starting point"
      )
      <> Options.Applicative.long "defaults"

format :: Options.Applicative.Parser Format
format =
  pure Format'
    <*> input
    <*> output
    <*> verbose

info :: Options.Applicative.ParserInfo Args
info = Options.Applicative.info (Options.Applicative.helper <*> args) description
  where
  description :: Options.Applicative.InfoMod Args
  description =
    Options.Applicative.fullDesc
      <> Options.Applicative.progDesc "Pretty print a PureScript file"
      <> Options.Applicative.header "purty - A PureScript pretty-printer"

input :: Options.Applicative.Parser Input
input =
  Options.Applicative.argument input' meta
  where
  meta :: Options.Applicative.Mod Options.Applicative.ArgumentFields a
  meta =
    Options.Applicative.help "PureScript file to format or `-` for STDIN"
      <> Options.Applicative.metavar "FILE"

  input' :: Options.Applicative.ReadM Input
  input' = Options.Applicative.maybeReader $ \str -> case str of
    "-" -> Just InputSTDIN
    _   -> Just (InputFile str)

output :: Options.Applicative.Parser Output
output = Options.Applicative.flag STDOUT Write meta
  where
  meta :: Options.Applicative.Mod Options.Applicative.FlagFields a
  meta =
    Options.Applicative.help "Format file in-place"
      <> Options.Applicative.long "write"

outputInputType :: Dhall.InputType Output
outputInputType = Dhall.InputType { Dhall.declared, Dhall.embed }
  where
  declared :: Dhall.Core.Expr Dhall.Parser.Src Dhall.TypeCheck.X
  declared =
    Dhall.Core.Union
      ( Dhall.Map.fromList
        [ ("STDOUT", Nothing)
        , ("Write", Nothing)
        ]
      )

  embed :: Output -> Dhall.Core.Expr Dhall.Parser.Src Dhall.TypeCheck.X
  embed output' = case output' of
    STDOUT -> Dhall.Core.Field declared "STDOUT"
    Write  -> Dhall.Core.Field declared "Write"

outputType :: Dhall.Type Output
outputType = Dhall.Type { Dhall.expected, Dhall.extract }
  where
  expected :: Dhall.Core.Expr Dhall.Parser.Src Dhall.TypeCheck.X
  expected =
    Dhall.Core.Union
      ( Dhall.Map.fromList
        [ ("STDOUT", Nothing)
        , ("Write", Nothing)
        ]
      )

  extract :: Dhall.Core.Expr Dhall.Parser.Src Dhall.TypeCheck.X -> Maybe Output
  extract expr = case expr of
    Dhall.Core.Field union "STDOUT"
      | union == expected -> Just STDOUT
    Dhall.Core.Field union "Write"
      | union == expected -> Just Write
    _ -> Nothing

parse :: IO Args
parse = do
  args' <- Options.Applicative.execParser info
  let config' =
        Log.Config
          { Log.name = "Log - config parser"
          , Log.verbose = debug args'
          }
  runComponent (debug args') "purty" (Log.handle config') $ \log ->
    withConfig log args'
  where
  runComponent ::
    Bool ->
    Text ->
    Control.Monad.Component.ComponentM a ->
    (a -> IO Args) ->
    IO Args
  runComponent debug'
    | debug' =
      Control.Monad.Component.runComponentM1 (runSimpleApp . logInfo . display)
    | otherwise =
      Control.Monad.Component.runComponentM

verbose :: Options.Applicative.Parser Verbose
verbose = Options.Applicative.flag NotVerbose Verbose meta
  where
  meta :: Options.Applicative.Mod Options.Applicative.FlagFields a
  meta =
    Options.Applicative.help "Print debugging information to STDERR while running"
      <> Options.Applicative.long "verbose"

verboseInputType :: Dhall.InputType Verbose
verboseInputType = Dhall.InputType { Dhall.declared, Dhall.embed }
  where
  declared :: Dhall.Core.Expr Dhall.Parser.Src Dhall.TypeCheck.X
  declared =
    Dhall.Core.Union
      ( Dhall.Map.fromList
        [ ("NotVerbose", Nothing)
        , ("Verbose", Nothing)
        ]
      )

  embed :: Verbose -> Dhall.Core.Expr Dhall.Parser.Src Dhall.TypeCheck.X
  embed verbose' = case verbose' of
    NotVerbose -> Dhall.Core.Field declared "NotVerbose"
    Verbose    -> Dhall.Core.Field declared "Verbose"

verboseType :: Dhall.Type Verbose
verboseType = Dhall.Type { Dhall.expected, Dhall.extract }
  where
  expected :: Dhall.Core.Expr Dhall.Parser.Src Dhall.TypeCheck.X
  expected =
    Dhall.Core.Union
      ( Dhall.Map.fromList
        [ ("NotVerbose", Nothing)
        , ("Verbose", Nothing)
        ]
      )

  extract :: Dhall.Core.Expr Dhall.Parser.Src Dhall.TypeCheck.X -> Maybe Verbose
  extract expr = case expr of
    Dhall.Core.Field union "NotVerbose"
      | union == expected -> Just NotVerbose
    Dhall.Core.Field union "Verbose"
      | union == expected -> Just Verbose
    _ -> Nothing

withConfig :: Log.Handle -> Args -> IO Args
withConfig log args' = case args' of
  Defaults defaults' -> do
    Log.debug log "Defaults were asked for, not parsing the config file."
    pure (Defaults defaults')
  Format (Format' input' output'' verbose'') -> do
    let file = "./.purty.dhall"
    Log.debug log ("Parsing " <> displayShow file <> ".")
    result <- tryAny (Dhall.inputFile configType file)
    case result of
      Left err -> do
        Log.debug
          log
          ("Could not parse " <> displayShow file <> ". " <> displayShow err)
        pure (Format $ Format' input' output'' verbose'')
      Right config'@(Config output' verbose') -> do
        Log.debug log ("Parsed " <> displayShow file <> ". " <> display config')
        let format' =
              Format' input' (output' <> output'') (verbose' <> verbose'')
        Log.debug log ("Defaulted with config values " <> display format')
        pure (Format format')

withInput ::
  Log.Handle ->
  Format ->
  (LByteString -> IO a) ->
  IO (Either Error.Error a)
withInput log format' f = case format' of
  Format' (InputFile file) _ _ -> do
    Log.debug log ("Reading " <> displayShow file <> ".")
    result' <- tryIO $ withLazyFile file $ \contents -> do
      Log.debug log "Got the file contents"
      result <- f contents
      Log.debug log "Finished with the file"
      pure result
    case result' of
      Left err ->
        pure (Left $ Error.new $ "Error reading file: " <> displayShow err)
      Right result ->
        pure (Right result)
  Format' InputSTDIN _ _ -> do
    Log.debug log "Reading STDIN."
    result' <- tryIO RIO.ByteString.Lazy.getContents
    case result' of
      Left err ->
        pure (Left $ Error.new $ "Error reading file: " <> displayShow err)
      Right contents -> do
        Log.debug log "Got STDIN contents"
        result <- f contents
        pure (Right result)

write :: Log.Handle -> Format -> Utf8Builder -> IO ()
write log format' formatted = case format' of
  Format' _ STDOUT _ -> do
    Log.debug log "Writing formatted file to STDOUT"
    hPutBuilder stdout (getUtf8Builder formatted)
    Log.debug log "Wrote formatted file to STDOUT"
  Format' (InputFile file') Write _ -> do
    Log.debug log ("Converting file " <> displayShow file' <> " to absolute.")
    file <- RIO.Directory.makeAbsolute file'
    Log.debug log ("Writing formatted file " <> displayShow file <> " in-place.")
    RIO.File.writeBinaryFileDurableAtomic
      file
      ( toStrictBytes
        $ Data.ByteString.Builder.toLazyByteString
        $ getUtf8Builder formatted
      )
    Log.debug log "Wrote formatted file in-place"
  Format' InputSTDIN _ _ -> do
    Log.debug log "Writing formatted STDIN to STDOUT"
    hPutBuilder stdout (getUtf8Builder formatted)
    Log.debug log "Wrote formatted STDIN to STDOUT"

writeDefaults :: Log.Handle -> Defaults -> IO ()
writeDefaults log defaults'' = case defaults'' of
  Defaults' _ -> do
    Log.debug log "Generating defaults"
    let defaults' =
          display (Dhall.Core.pretty $ Dhall.embed configInputType config)
    Log.debug log ("Generated defaults: " <> defaults')
    Log.debug log "Writing defaults to STDOUT"
    hPutBuilder stdout (getUtf8Builder defaults')
