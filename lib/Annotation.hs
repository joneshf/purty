module Annotation
  ( module'
  ) where

import "rio" RIO hiding (log, span)

import qualified "purescript" Language.PureScript.CST
import qualified "this" Log
import qualified "this" Span

dataMembers ::
  Log.Handle ->
  Language.PureScript.CST.DataMembers a ->
  IO (Language.PureScript.CST.DataMembers Span.Span)
dataMembers log dataMembers' = case dataMembers' of
  Language.PureScript.CST.DataAll _ sourceToken' -> do
    let span = Span.SingleLine
    Log.debug log ("Annotating `DataAll` as `" <> displayShow span <> "`")
    pure (Language.PureScript.CST.DataAll span sourceToken')
  Language.PureScript.CST.DataEnumerated _ delimited' -> do
    let span = Span.dataMembers dataMembers'
    Log.debug log ("Annotating `DataEnumerated` as `" <> displayShow span <> "`")
    pure (Language.PureScript.CST.DataEnumerated span delimited')

declaration ::
  Log.Handle ->
  Language.PureScript.CST.Declaration a ->
  IO (Language.PureScript.CST.Declaration Span.Span)
declaration log declaration' = do
  let span = Span.MultipleLines
  Log.info
    log
    ( "Annotating `Declaration` not implemented. Defaulting to `"
      <> displayShow span
      <> "`"
    )
  pure (span <$ declaration')

export ::
  Log.Handle ->
  Language.PureScript.CST.Export a ->
  IO (Language.PureScript.CST.Export Span.Span)
export log export' = case export' of
  Language.PureScript.CST.ExportClass _ class' name' -> do
    Log.debug log (debug "ExportClass" name' span)
    pure (Language.PureScript.CST.ExportClass span class' name')
  Language.PureScript.CST.ExportKind _ kind' name' -> do
    Log.debug log (debug "ExportKind" name' span)
    pure (Language.PureScript.CST.ExportKind span kind' name')
  Language.PureScript.CST.ExportModule _ module'' name' -> do
    Log.debug log (debug "ExportModule" name' span)
    pure (Language.PureScript.CST.ExportModule span module'' name')
  Language.PureScript.CST.ExportOp _ name' -> do
    Log.debug log (debug "ExportOp" name' span)
    pure (Language.PureScript.CST.ExportOp span name')
  Language.PureScript.CST.ExportType _ name' dataMembers'' -> do
    Log.debug log (debug "ExportType" name' span)
    dataMembers' <- traverse (dataMembers log) dataMembers''
    pure (Language.PureScript.CST.ExportType span name' dataMembers')
  Language.PureScript.CST.ExportTypeOp _ type'' name' -> do
    Log.debug log (debug "ExportTypeOp" name' span)
    pure (Language.PureScript.CST.ExportTypeOp span type'' name')
  Language.PureScript.CST.ExportValue _ name' -> do
    Log.debug log (debug "ExportValue" name' span)
    pure (Language.PureScript.CST.ExportValue span name')
  where
  debug ::
    (Show a) =>
    Utf8Builder ->
    Language.PureScript.CST.Name a ->
    Span.Span ->
    Utf8Builder
  debug x y z =
    "Annotating `"
      <> x
      <> "`: "
      <> displayShow y
      <> " as `"
      <> displayShow z
      <> "`"

  span :: Span.Span
  span = Span.export export'

import' ::
  Log.Handle ->
  Language.PureScript.CST.Import a ->
  IO (Language.PureScript.CST.Import Span.Span)
import' log import'' = case import'' of
  Language.PureScript.CST.ImportClass _ class' name' -> do
    Log.debug log (debug "ImportClass" name' span)
    pure (Language.PureScript.CST.ImportClass span class' name')
  Language.PureScript.CST.ImportKind _ kind' name' -> do
    Log.debug log (debug "ImportKind" name' span)
    pure (Language.PureScript.CST.ImportKind span kind' name')
  Language.PureScript.CST.ImportOp _ name' -> do
    Log.debug log (debug "ImportOp" name' span)
    pure (Language.PureScript.CST.ImportOp span name')
  Language.PureScript.CST.ImportType _ name' dataMembers'' -> do
    Log.debug log (debug "ImportType" name' span)
    dataMembers' <- traverse (dataMembers log) dataMembers''
    pure (Language.PureScript.CST.ImportType span name' dataMembers')
  Language.PureScript.CST.ImportTypeOp _ type'' name' -> do
    Log.debug log (debug "ImportTypeOp" name' span)
    pure (Language.PureScript.CST.ImportTypeOp span type'' name')
  Language.PureScript.CST.ImportValue _ name' -> do
    Log.debug log (debug "ImportValue" name' span)
    pure (Language.PureScript.CST.ImportValue span name')
  where
  debug ::
    (Show a) =>
    Utf8Builder ->
    Language.PureScript.CST.Name a ->
    Span.Span ->
    Utf8Builder
  debug x y z =
    "Annotating `"
      <> x
      <> "`: "
      <> displayShow y
      <> " as `"
      <> displayShow z
      <> "`"

  span :: Span.Span
  span = Span.import' import''

importDecl ::
  Log.Handle ->
  Language.PureScript.CST.ImportDecl a ->
  IO (Language.PureScript.CST.ImportDecl Span.Span)
importDecl log importDecl' = case importDecl' of
  Language.PureScript.CST.ImportDecl _ import'' name' imports' rename -> do
    let span = Span.importDecl importDecl'
    Log.debug log ("Annotating `ImportDecl` as `" <> displayShow span <> "`")
    imports <- (traverse . traverse . traverse . traverse) (import' log) imports'
    pure (Language.PureScript.CST.ImportDecl span import'' name' imports rename)

module' ::
  Log.Handle ->
  Language.PureScript.CST.Module a ->
  IO (Language.PureScript.CST.Module Span.Span)
module' log module''' = case module''' of
  Language.PureScript.CST.Module _ module'' name exports' where'' imports' declarations' trailing -> do
    let span = Span.betweenSourceTokens module'' where''
    Log.debug log ("Annotating `Module` as `" <> displayShow span <> "`")
    exports <- (traverse . traverse . traverse) (export log) exports'
    imports <- traverse (importDecl log) imports'
    declarations <- traverse (declaration log) declarations'
    pure
      ( Language.PureScript.CST.Module
        span
        module''
        name
        exports
        where''
        imports
        declarations
        trailing
      )
