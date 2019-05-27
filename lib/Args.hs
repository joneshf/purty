module Args
  ( Args
  , debug
  , info
  ) where

import "rio" RIO

import qualified "optparse-applicative" Options.Applicative

data Args
  = Format Format

data Format
  = Format' Verbose

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
  Format' verbose' -> debugVerbose verbose'

debugVerbose :: Verbose -> Bool
debugVerbose verbose' = case verbose' of
  NotVerbose -> False
  Verbose -> True

format :: Options.Applicative.Parser Format
format = fmap Format' verbose

info :: Options.Applicative.ParserInfo Args
info =
  Options.Applicative.info
    (Options.Applicative.helper <*> args)
    ( Options.Applicative.fullDesc
      <> Options.Applicative.progDesc "Pretty print a PureScript file"
      <> Options.Applicative.header "purty - A PureScript pretty-printer"
    )

verbose :: Options.Applicative.Parser Verbose
verbose = Options.Applicative.flag NotVerbose Verbose meta
  where
  meta :: Options.Applicative.Mod Options.Applicative.FlagFields a
  meta =
    Options.Applicative.help "Print debugging information to STDERR while running"
      <> Options.Applicative.long "verbose"
