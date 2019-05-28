module Annotation
  ( module'
  ) where

import "rio" RIO hiding (log)

import qualified "purescript" Language.PureScript.CST
import qualified "this" Log
import qualified "this" Span

module' ::
  Log.Handle ->
  Language.PureScript.CST.Module a ->
  IO (Language.PureScript.CST.Module Span.Span)
module' log module''' = case module''' of
  Language.PureScript.CST.Module _ _module'' _name _exports' _where'' _imports' _declarations' _trailing -> do
    Log.info log "Annotating `Module` not implemented."
    pure (Span.MultipleLines <$ module''')
