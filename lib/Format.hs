module Format
  ( module'
  ) where

import "rio" RIO hiding (log)

import qualified "purescript" Language.PureScript.CST
import qualified "this" Span
import qualified "this" Log

blank :: Utf8Builder
blank = ""

module' ::
  Log.Handle ->
  Language.PureScript.CST.Module Span.Span ->
  IO Utf8Builder
module' log module''' = case module''' of
  Language.PureScript.CST.Module _ _module'' _name _exports' _where'' _imports' _declarations' _trailing -> do
    Log.info log "Formatting `Module` not implemented."
    pure blank
