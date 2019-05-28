module Args
  ( Args(..)
  , debug
  , info
  , withInput
  , write
  ) where

import "rio" RIO hiding (log)

import qualified "bytestring" Data.ByteString.Builder
import qualified "this" Log
import qualified "optparse-applicative" Options.Applicative
import qualified "rio" RIO.File

data Args
  = Format Format

data Format
  = Format' Input Output Verbose

data Input
  = InputFile FilePath

data Output
  = STDOUT
  | Write

data Verbose
  = NotVerbose
  | Verbose

args :: Options.Applicative.Parser Args
args = fmap Format format

debug :: Args -> Bool
debug args' = case args' of
  Format format' -> debugFormat format'

debugFormat :: Format -> Bool
debugFormat format' = case format' of
  Format' _ _ verbose' -> debugVerbose verbose'

debugVerbose :: Verbose -> Bool
debugVerbose verbose' = case verbose' of
  NotVerbose -> False
  Verbose    -> True

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
input = fmap InputFile (Options.Applicative.strArgument meta)
  where
  meta :: Options.Applicative.Mod Options.Applicative.ArgumentFields FilePath
  meta =
    Options.Applicative.help "PureScript file to format"
      <> Options.Applicative.metavar "FILE"

output :: Options.Applicative.Parser Output
output = Options.Applicative.flag STDOUT Write meta
  where
  meta :: Options.Applicative.Mod Options.Applicative.FlagFields a
  meta =
    Options.Applicative.help "Format file in-place"
      <> Options.Applicative.long "write"


verbose :: Options.Applicative.Parser Verbose
verbose = Options.Applicative.flag NotVerbose Verbose meta
  where
  meta :: Options.Applicative.Mod Options.Applicative.FlagFields a
  meta =
    Options.Applicative.help "Print debugging information to STDERR while running"
      <> Options.Applicative.long "verbose"

withInput :: Log.Handle -> Format -> (LByteString -> IO a) -> IO a
withInput log format' f = case format' of
  Format' (InputFile file) _ _ -> do
    Log.debug log ("Reading " <> displayShow file <> ".")
    withLazyFile file $ \contents -> do
      Log.debug log "Got the file contents"
      result <- f contents
      Log.debug log "Finished with the file"
      pure result

write :: Log.Handle -> Format -> Utf8Builder -> IO ()
write log format' formatted = case format' of
  Format' _ STDOUT _ -> do
    Log.debug log "Writing formatted file to STDOUT"
    hPutBuilder stdout (getUtf8Builder formatted)
    Log.debug log "Wrote formatted file to STDOUT"
  Format' (InputFile file) Write _ -> do
    Log.debug log "Writing formatted file in-place"
    RIO.File.writeBinaryFileDurableAtomic
      file
      ( toStrictBytes
        $ Data.ByteString.Builder.toLazyByteString
        $ getUtf8Builder formatted
      )
    Log.debug log "Wrote formatted file in-place"
