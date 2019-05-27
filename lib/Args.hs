module Args
  ( Args
  , debug
  , info
  ) where

import "rio" RIO

import qualified "optparse-applicative" Options.Applicative

data Args
  = Format

args :: Options.Applicative.Parser Args
args = pure Format

debug :: Args -> Bool
debug args' = case args' of
  Format -> True

info :: Options.Applicative.ParserInfo Args
info =
  Options.Applicative.info
    (Options.Applicative.helper <*> args)
    ( Options.Applicative.fullDesc
      <> Options.Applicative.progDesc "Pretty print a PureScript file"
      <> Options.Applicative.header "purty - A PureScript pretty-printer"
    )
