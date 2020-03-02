{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Args
  ( Args (..),
    Mode (..),
    debug,
    parse,
    withInput,
    withValidate,
    writeVersion,
  )
where

import qualified "bytestring" Data.ByteString.Builder
import qualified "this" Error
import qualified "this" Log
import qualified "optparse-applicative" Options.Applicative
import "rio" RIO hiding (log)
import qualified "rio" RIO.ByteString.Lazy
import qualified "rio" RIO.Directory
import qualified "rio" RIO.File
import "rio" RIO.FilePath ((</>))
import qualified "rio" RIO.FilePath
import qualified "pathwalk" System.Directory.PathWalk
import qualified "this" Version

data Args
  = Args Mode Verbose

data Format
  = Format' Input Output

data Input
  = InputFile FilePath
  | InputSTDIN

data Mode
  = Format Format
  | Validate Validate
  | Version Version

data Output
  = STDOUT
  | Write

newtype Validate
  = Validate' Input

data Verbose
  = NotVerbose
  | Verbose

newtype Version
  = Version' VersionFormat

data VersionFormat
  = VersionHuman
  | VersionNumeric

args :: Options.Applicative.Parser Args
args =
  pure Args
    <*> mode
    <*> verbose

debug :: Args -> Bool
debug args' = case args' of
  Args _ verbose' -> debugVerbose verbose'

debugVerbose :: Verbose -> Bool
debugVerbose verbose' = case verbose' of
  NotVerbose -> False
  Verbose -> True

formatParser :: Options.Applicative.Parser Format
formatParser =
  pure Format'
    <*> inputParser
    <*> output
  where
    inputArgumentFields :: Options.Applicative.Mod Options.Applicative.ArgumentFields a
    inputArgumentFields =
      Options.Applicative.help "PureScript file to format or `-` for STDIN"
        <> Options.Applicative.metavar "FILE"
    inputParser :: Options.Applicative.Parser Input
    inputParser =
      Options.Applicative.argument inputReader inputArgumentFields

formatParserInfo :: Options.Applicative.ParserInfo Format
formatParserInfo = Options.Applicative.info formatParser description
  where
    description :: Options.Applicative.InfoMod Format
    description =
      Options.Applicative.fullDesc
        <> Options.Applicative.progDesc "Format a PureScript file"

info :: Options.Applicative.ParserInfo Args
info = Options.Applicative.info (Options.Applicative.helper <*> args) description
  where
    description :: Options.Applicative.InfoMod Args
    description =
      Options.Applicative.fullDesc
        <> Options.Applicative.progDesc "Pretty print a PureScript file"
        <> Options.Applicative.header "purty - A PureScript pretty-printer"

inputReader :: Options.Applicative.ReadM Input
inputReader = Options.Applicative.maybeReader $ \str -> case str of
  "-" -> Just InputSTDIN
  _ -> Just (InputFile str)

mode :: Options.Applicative.Parser Mode
mode =
  asum
    [ Options.Applicative.hsubparser
        ( fold
            [ Options.Applicative.command "format" (fmap Format formatParserInfo),
              Options.Applicative.command "validate" (fmap Validate validateParserInfo),
              Options.Applicative.command "version" (fmap Version versionParserInfo)
            ]
        ),
      fmap Format formatParser
    ]

output :: Options.Applicative.Parser Output
output = Options.Applicative.flag STDOUT Write meta
  where
    meta :: Options.Applicative.Mod Options.Applicative.FlagFields a
    meta =
      Options.Applicative.help "Format file in-place"
        <> Options.Applicative.long "write"

parse :: IO Args
parse = Options.Applicative.execParser info

validateFile ::
  Log.Handle ->
  (LByteString -> IO (Either Error.Error Utf8Builder)) ->
  FilePath ->
  IO (Maybe Error.Error)
validateFile log f file = do
  Log.debug log ("Reading " <> displayShow file <> ".")
  result' <- tryIO $ withLazyFile file $ \contents -> do
    Log.debug log "Got the file contents. Formatting for validation"
    result <- f contents
    Log.debug log "Finished with the file"
    pure (contents, result)
  case result' of
    Left err ->
      pure (Just $ Error.new $ "Error reading file: " <> displayShow err)
    Right result -> case result of
      (_, Left err) ->
        pure (Just (Error.wrap ("Error formatting " <> displayShow file) err))
      (contents, Right formatted) -> do
        Log.debug log ("Comparing " <> displayShow file <> " with formatted module")
        if contents == Data.ByteString.Builder.toLazyByteString (getUtf8Builder formatted)
          then do
            Log.debug log (displayShow file <> " is formatted correctly")
            pure Nothing
          else do
            Log.debug log (displayShow file <> " is not formatted correctly")
            pure (Just (Error.new (displayShow file <> " is not formatted correctly")))

validateFiles ::
  Log.Handle ->
  (LByteString -> IO (Either Error.Error Utf8Builder)) ->
  FilePath ->
  [FilePath] ->
  IO [Error.Error]
validateFiles log f directory files = do
  Log.debug log ("Validating the following files: " <> displayShow files <> " in directory: " <> displayShow directory)
  errors <- traverse go files
  pure (catMaybes errors)
  where
    go ::
      FilePath ->
      IO (Maybe Error.Error)
    go file' = case pureScriptFile file' of
      Just file -> do
        Log.debug log ("Converting file " <> displayShow (directory </> file) <> " to absolute")
        absoluteFile <- RIO.Directory.makeAbsolute (directory </> file)
        validateFile log f absoluteFile
      Nothing -> pure Nothing
    pureScriptFile ::
      FilePath ->
      Maybe FilePath
    pureScriptFile file
      | RIO.FilePath.isExtensionOf "purs" file = Just file
      | otherwise = Nothing

validateParser :: Options.Applicative.Parser Validate
validateParser =
  pure Validate'
    <*> inputParser
  where
    inputArgumentFields :: Options.Applicative.Mod Options.Applicative.ArgumentFields a
    inputArgumentFields =
      Options.Applicative.help "PureScript file to validate or `-` for STDIN"
        <> Options.Applicative.metavar "FILE"
    inputParser :: Options.Applicative.Parser Input
    inputParser =
      Options.Applicative.argument inputReader inputArgumentFields

validateParserInfo :: Options.Applicative.ParserInfo Validate
validateParserInfo = Options.Applicative.info validateParser description
  where
    description :: Options.Applicative.InfoMod Validate
    description =
      Options.Applicative.progDesc "Validate formatting of a PureScript file"

verbose :: Options.Applicative.Parser Verbose
verbose = Options.Applicative.flag NotVerbose Verbose meta
  where
    meta :: Options.Applicative.Mod Options.Applicative.FlagFields a
    meta =
      Options.Applicative.help "Print debugging information to STDERR while running"
        <> Options.Applicative.long "verbose"

versionParserInfo :: Options.Applicative.ParserInfo Version
versionParserInfo = Options.Applicative.info versionParser description
  where
    description :: Options.Applicative.InfoMod Version
    description =
      Options.Applicative.progDesc "Print version information"

versionParser :: Options.Applicative.Parser Version
versionParser =
  pure Version'
    <*> versionFormat

versionFormat :: Options.Applicative.Parser VersionFormat
versionFormat =
  asum
    [ pure VersionHuman,
      Options.Applicative.flag' VersionNumeric versionNumeric
    ]
  where
    versionNumeric :: Options.Applicative.Mod Options.Applicative.FlagFields a
    versionNumeric =
      Options.Applicative.help "Print machine-readable version number only"
        <> Options.Applicative.long "numeric"

whenDirectory ::
  Log.Handle ->
  FilePath ->
  (FilePath -> [FilePath] -> IO [Error.Error]) ->
  IO (Maybe [Error.Error])
whenDirectory log file f = do
  directoryExists <- RIO.Directory.doesDirectoryExist file
  if directoryExists
    then do
      Log.debug log ("Parsed " <> displayShow file <> " as an absolute directory")
      errors <- System.Directory.PathWalk.pathWalkAccumulate file (\directory _ files -> f directory files)
      pure (Just errors)
    else pure Nothing

withInput ::
  Log.Handle ->
  Format ->
  (LByteString -> IO (Either Error.Error Utf8Builder)) ->
  IO [Error.Error]
withInput log format' f = case format' of
  Format' (InputFile file') output' -> do
    Log.debug log ("Converting file " <> displayShow file' <> " to absolute.")
    file <- RIO.Directory.makeAbsolute file'
    directoryErrors <- whenDirectory log file (writeFiles log f output')
    case directoryErrors of
      Just errors -> pure errors
      Nothing -> do
        err' <- write log f output' file
        case err' of
          Just err -> pure [err]
          Nothing -> pure []
  Format' InputSTDIN _ -> do
    Log.debug log "Reading STDIN."
    result' <- tryIO RIO.ByteString.Lazy.getContents
    case result' of
      Left err ->
        pure [Error.new ("Error reading STDIN: " <> displayShow err)]
      Right contents -> do
        Log.debug log "Got STDIN contents"
        result <- f contents
        case result of
          Left err ->
            pure [Error.wrap "Error formatting STDIN" err]
          Right formatted -> do
            Log.debug log "Writing formatted STDIN to STDOUT"
            hPutBuilder stdout (getUtf8Builder formatted)
            Log.debug log "Wrote formatted STDIN to STDOUT"
            pure []

withValidate ::
  Log.Handle ->
  Validate ->
  (LByteString -> IO (Either Error.Error Utf8Builder)) ->
  IO [Error.Error]
withValidate log validate' f = case validate' of
  Validate' (InputFile file') -> do
    Log.debug log ("Converting file " <> displayShow file' <> " to absolute.")
    file <- RIO.Directory.makeAbsolute file'
    directoryErrors <- whenDirectory log file (validateFiles log f)
    case directoryErrors of
      Just errors -> pure errors
      Nothing -> do
        err' <- validateFile log f file
        case err' of
          Just err -> pure [err]
          Nothing -> pure []
  Validate' InputSTDIN -> do
    Log.debug log "Reading STDIN."
    result' <- tryIO RIO.ByteString.Lazy.getContents
    case result' of
      Left err ->
        pure [Error.new ("Error reading STDIN: " <> displayShow err)]
      Right contents -> do
        Log.debug log "Got STDIN contents"
        result <- f contents
        case result of
          Left err ->
            pure [Error.wrap "Error validating STDIN" err]
          Right formatted -> do
            Log.debug log "Comparing STDIN with formatted module"
            if contents == Data.ByteString.Builder.toLazyByteString (getUtf8Builder formatted)
              then do
                Log.debug log "STDIN was formatted correctly"
                pure []
              else do
                Log.debug log "STDIN was not formatted correctly"
                pure [Error.new "Module was not formatted correctly"]

write ::
  Log.Handle ->
  (LByteString -> IO (Either Error.Error Utf8Builder)) ->
  Output ->
  FilePath ->
  IO (Maybe Error.Error)
write log f output' file = do
  Log.debug log ("Reading " <> displayShow file <> ".")
  result' <- tryIO $ withLazyFile file $ \contents -> do
    Log.debug log "Got the file contents"
    result <- f contents
    Log.debug log "Finished with the file"
    pure result
  case result' of
    Left err ->
      pure (Just $ Error.new $ "Error reading file: " <> displayShow err)
    Right result -> case result of
      Left err ->
        pure (Just (Error.wrap ("Error formatting " <> displayShow file) err))
      Right formatted -> case output' of
        STDOUT -> do
          Log.debug log "Writing formatted file to STDOUT"
          hPutBuilder stdout (getUtf8Builder formatted)
          Log.debug log "Wrote formatted file to STDOUT"
          pure Nothing
        Write -> do
          Log.debug log ("Writing formatted file " <> displayShow file <> " in-place.")
          RIO.File.withBinaryFileDurableAtomic
            file
            WriteMode
            (\h -> hPutBuilder h (getUtf8Builder formatted))
          Log.debug log "Wrote formatted file in-place"
          pure Nothing

writeFiles ::
  Log.Handle ->
  (LByteString -> IO (Either Error.Error Utf8Builder)) ->
  Output ->
  FilePath ->
  [FilePath] ->
  IO [Error.Error]
writeFiles log f output' directory files = do
  Log.debug log ("Writing the following files: " <> displayShow files <> " in directory: " <> displayShow directory)
  errors <- traverse go files
  pure (catMaybes errors)
  where
    go ::
      FilePath ->
      IO (Maybe Error.Error)
    go file' = case pureScriptFile file' of
      Just file -> do
        Log.debug log ("Converting file " <> displayShow (directory </> file) <> " to absolute")
        absoluteFile <- RIO.Directory.makeAbsolute (directory </> file)
        write log f output' absoluteFile
      Nothing -> pure Nothing
    pureScriptFile ::
      FilePath ->
      Maybe FilePath
    pureScriptFile file
      | RIO.FilePath.isExtensionOf "purs" file = Just file
      | otherwise = Nothing

writeVersion ::
  Log.Handle ->
  Version ->
  IO ()
writeVersion log version' = case version' of
  Version' VersionHuman -> do
    Log.debug log "Writing version information"
    hPutBuilder stdout ("Purty version: " <> Version.version <> "\n")
  Version' VersionNumeric -> do
    Log.debug log "Writing only `purty` version number"
    hPutBuilder stdout (Version.version <> "\n")
