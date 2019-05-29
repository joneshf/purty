module Format
  ( module'
  ) where

import "rio" RIO hiding (log, span)

import qualified "purescript" Language.PureScript.CST
import qualified "this" Span
import qualified "this" Log

type Indent
  = Utf8Builder

type Indentation
  = Utf8Builder

type Prefix
  = Utf8Builder

blank :: Utf8Builder
blank = ""

commentLeading ::
  Log.Handle ->
  Indent ->
  Prefix ->
  Language.PureScript.CST.Comment a ->
  IO Utf8Builder
commentLeading log indent prefix comment'' = case comment'' of
  Language.PureScript.CST.Comment comment' -> do
    Log.debug log ("Formatting comment: " <> display comment')
    pure (prefix <> display comment' <> newline <> indent)
  Language.PureScript.CST.Line _ -> do
    Log.debug log "Not formatting `Line`"
    pure blank
  Language.PureScript.CST.Space _ -> do
    Log.debug log "Not formatting `Space`"
    pure blank

commentTrailing ::
  Log.Handle ->
  Prefix ->
  Language.PureScript.CST.Comment a ->
  IO Utf8Builder
commentTrailing log prefix comment'' = case comment'' of
  Language.PureScript.CST.Comment comment' -> do
    Log.debug log ("Formatting comment: " <> display comment')
    pure (prefix <> display comment')
  Language.PureScript.CST.Line _ -> do
    Log.debug log "Not formatting `Line`"
    pure blank
  Language.PureScript.CST.Space _ -> do
    Log.debug log "Not formatting `Space`"
    pure blank

commentsLeading ::
  Log.Handle ->
  Indent ->
  Prefix ->
  [Language.PureScript.CST.Comment a] ->
  IO Utf8Builder
commentsLeading log indent prefix commentsLeading' = case commentsLeading' of
  [] -> do
    Log.debug log "No leading comments to format"
    pure blank
  _ -> do
    Log.debug log "Formatting leading comments"
    foldMap (commentLeading log indent prefix) commentsLeading'

commentsTrailing ::
  Log.Handle ->
  Prefix ->
  [Language.PureScript.CST.Comment a] ->
  IO Utf8Builder
commentsTrailing log prefix commentsTrailing' = case commentsTrailing' of
  [] -> do
    Log.debug log "No trailing comments to format"
    pure blank
  _ -> do
    Log.debug log "Formatting trailing comments"
    foldMap (commentTrailing log prefix) commentsTrailing'

dataMembers ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.DataMembers Span.Span ->
  IO Utf8Builder
dataMembers log indentation indent' dataMembers' = case dataMembers' of
  Language.PureScript.CST.DataAll _ sourceToken' -> do
    Log.debug log "Formatting `DataAll`"
    sourceToken log indent' blank sourceToken'
  Language.PureScript.CST.DataEnumerated span delimited' -> do
    let
      (indent, prefix) = case span of
        Span.MultipleLines ->
          (indent' <> indentation, newline <> indent')
        Span.SingleLine ->
          (indent', blank)
    Log.debug log ("Formatting `DataEnumerated` as `" <> displayShow span <> "`")
    pure prefix
      <> delimited
        log
        indent'
        Span.sourceRangeFromName
        (name log indent blank)
        delimited'

delimited ::
  Log.Handle ->
  Indent ->
  (a -> Language.PureScript.CST.SourceRange) ->
  (a -> IO Utf8Builder) ->
  Language.PureScript.CST.Delimited a ->
  IO Utf8Builder
delimited log indent f g delimited' = do
  Log.debug log "Formatting `Delimited`"
  wrapped log indent (foldMap $ separated log indent f g) delimited'

delimitedNonEmpty ::
  Log.Handle ->
  Indent ->
  (a -> Language.PureScript.CST.SourceRange) ->
  (a -> IO Utf8Builder) ->
  Language.PureScript.CST.DelimitedNonEmpty a ->
  IO Utf8Builder
delimitedNonEmpty log indent f g delimitedNonEmpty' = do
  Log.debug log "Formatting `DelimitedNonEmpty`"
  wrapped log indent (separated log indent f g) delimitedNonEmpty'

export ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Export Span.Span ->
  IO Utf8Builder
export log indentation indent' export' = case export' of
  Language.PureScript.CST.ExportClass _ class' name' -> do
    Log.debug log ("Formatting `ExportClass`: " <> displayShow name')
    sourceToken log indent' blank class'
      <> name log indent' space name'
  Language.PureScript.CST.ExportKind _ kind' name' -> do
    Log.debug log ("Formatting `ExportKind`: " <> displayShow name')
    sourceToken log indent' blank kind'
      <> name log indent' space name'
  Language.PureScript.CST.ExportModule _ module'' name' -> do
    Log.debug log ("Formatting `ExportModule`: " <> displayShow name')
    sourceToken log indent' blank module''
      <> name log indent' space name'
  Language.PureScript.CST.ExportOp _ name' -> do
    Log.debug log ("Formatting `ExportOp`: " <> displayShow name')
    name log indent' blank name'
  Language.PureScript.CST.ExportType span name' dataMembers' -> do
    let
      (indent, prefix) = case span of
        Span.MultipleLines ->
          (indent' <> indentation, newline <> indent)
        Span.SingleLine ->
          (indent', blank)
    Log.debug
      log
      ( "Formatting `ExportType`: "
        <> displayShow name'
        <> " as `"
        <> displayShow span
        <> "`"
      )
    name log indent' blank name'
      <> pure prefix
      <> foldMap (dataMembers log indentation indent) dataMembers'
  Language.PureScript.CST.ExportTypeOp _ type'' name' -> do
    Log.debug log ("Formatting `ExportTypeOp`: " <> displayShow name')
    sourceToken log indent' blank type''
      <> name log indent' space name'
  Language.PureScript.CST.ExportValue _ name' -> do
    Log.debug log ("Formatting `ExportValue`: " <> displayShow name')
    name log indent' blank name'

exports ::
  Log.Handle ->
  Indentation ->
  Maybe
    ( Language.PureScript.CST.DelimitedNonEmpty
      (Language.PureScript.CST.Export Span.Span)
    ) ->
  IO Utf8Builder
exports log indentation exports'' = case exports'' of
  Nothing -> do
    Log.debug log "No exports to format"
    pure blank
  Just exports' -> do
    let
      indent = indentation

      prefix = case span of
        Span.MultipleLines ->
          newline <> indent
        Span.SingleLine ->
          space

      span = Span.delimitedNonEmpty exports'
    Log.debug log ("Formatting exports as `" <> displayShow span <> "`")
    pure prefix
      <> delimitedNonEmpty
        log
        indent
        Span.sourceRangeFromExport
        (export log indentation indent)
        exports'

import' ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.Import Span.Span ->
  IO Utf8Builder
import' log indentation indent' import'' = case import'' of
  Language.PureScript.CST.ImportClass _ class' name' -> do
    Log.debug log ("Formatting `ImportClass`: " <> displayShow name')
    sourceToken log indent' blank class'
      <> name log indent' space name'
  Language.PureScript.CST.ImportKind _ kind' name' -> do
    Log.debug log ("Formatting `ImportKind`: " <> displayShow name')
    sourceToken log indent' blank kind'
      <> name log indent' space name'
  Language.PureScript.CST.ImportOp _ name' -> do
    Log.debug log ("Formatting `ImportOp`: " <> displayShow name')
    name log indent' blank name'
  Language.PureScript.CST.ImportType span name' dataMembers' -> do
    let
      indent = case span of
        Span.MultipleLines ->
          indent' <> indentation
        Span.SingleLine ->
          indent'
    Log.debug
      log
      ( "Formatting `ImportType`: "
        <> displayShow name'
        <> " as `"
        <> displayShow span
        <> "`"
      )
    name log indent' blank name'
      <> foldMap (dataMembers log indentation indent) dataMembers'
  Language.PureScript.CST.ImportTypeOp _ type'' name' -> do
    Log.debug log ("Formatting `ImportTypeOp`: " <> displayShow name')
    sourceToken log indent' blank type''
      <> name log indent' space name'
  Language.PureScript.CST.ImportValue _ name' -> do
    Log.debug log ("Formatting `ImportValue`: " <> displayShow name')
    name log indent' blank name'

importDecl ::
  Log.Handle ->
  Indentation ->
  Indent ->
  Language.PureScript.CST.ImportDecl Span.Span ->
  IO Utf8Builder
importDecl log indentation indent'' importDecl' = case importDecl' of
  Language.PureScript.CST.ImportDecl span import'' name'' imports'' rename -> do
    let indent' = indent'' <> indentation
    Log.debug log ("Formatting `ImportDecl` as `" <> displayShow span <> "`")
    sourceToken log indent'' blank import''
      <> name log indent'' space name''
      <> foldMap
        (\(hiding', imports') ->
          case hiding' of
            Just hiding -> do
              let
                hidingPrefix = case span of
                  Span.MultipleLines ->
                    newline <> indent'
                  Span.SingleLine ->
                    space

                importPrefix = case span of
                  Span.MultipleLines ->
                    newline <> indent
                  Span.SingleLine ->
                    space

                indent = indent' <> indentation
              pure hidingPrefix
                <> sourceToken log indent' blank hiding
                <> pure importPrefix
                <> delimitedNonEmpty
                  log
                  indent
                  Span.sourceRangeFromImport
                  (import' log indentation indent')
                  imports'
            Nothing -> do
              let
                importPrefix = case span of
                  Span.MultipleLines ->
                    newline <> indent'
                  Span.SingleLine ->
                    space
              pure importPrefix
                <> delimitedNonEmpty
                  log
                  indent'
                  Span.sourceRangeFromImport
                  (import' log indentation indent')
                  imports'
        )
        imports''
      <> foldMap
        (\(as, name') -> do
          let
            prefix = case span of
              Span.MultipleLines ->
                newline <> indent''
              Span.SingleLine ->
                space
          pure prefix
            <> sourceToken log indent' blank as
            <> name log indent' space name'
        )
        rename

imports ::
  Log.Handle ->
  Indentation ->
  [Language.PureScript.CST.ImportDecl Span.Span] ->
  IO Utf8Builder
imports log indentation imports' = case imports' of
  [] -> do
    Log.debug log "No imports to format"
    pure blank
  _ -> do
    let
      indent = blank
    Log.debug log "Formatting imports"
    foldMap
      (\importDecl' ->
        pure newline
          <> importDecl log indentation indent importDecl'
      )
      imports'
      <> pure newline

module' ::
  Log.Handle ->
  Indentation ->
  Language.PureScript.CST.Module Span.Span ->
  IO Utf8Builder
module' log indentation module''' = case module''' of
  Language.PureScript.CST.Module _ module'' name' exports' where'' imports' _declarations' _trailing -> do
    Log.info log "Formatting `Module` not implemented."
    sourceToken log blank blank module''
      <> name log blank space name'
      <> exports log indentation exports'
      <> sourceToken log blank space where''
      <> pure newline
      <> imports log indentation imports'

name ::
  Log.Handle ->
  Indent ->
  Prefix ->
  Language.PureScript.CST.Name a ->
  IO Utf8Builder
name log indent prefix name'' = case name'' of
  Language.PureScript.CST.Name name' _ -> do
    Log.debug log ("Formatting `Name`: " <> displayShow name')
    sourceToken log indent prefix name'

newline :: Utf8Builder
newline = "\n"

separated ::
  forall a.
  Log.Handle ->
  Indent ->
  (a -> Language.PureScript.CST.SourceRange) ->
  (a -> IO Utf8Builder) ->
  Language.PureScript.CST.Separated a ->
  IO Utf8Builder
separated log indent f g separated' = case separated' of
  Language.PureScript.CST.Separated head tail -> do
    Log.debug log ("Formatting `Separated` as " <> displayShow span <> "`")
    g head
      <> foldMap go tail
  where
  go :: (Language.PureScript.CST.SourceToken, a) -> IO Utf8Builder
  go x = case x of
    (separator, value) -> do
      let
        prefix = case span of
          Span.MultipleLines ->
            newline <> indent
          Span.SingleLine ->
            blank
      pure prefix
        <> sourceToken log indent blank separator
        <> pure space
        <> g value
  span :: Span.Span
  span = Span.separated f separated'

sourceToken ::
  Log.Handle ->
  Indent ->
  Prefix ->
  Language.PureScript.CST.SourceToken ->
  IO Utf8Builder
sourceToken log indent prefix sourceToken' = case sourceToken' of
  Language.PureScript.CST.SourceToken ann token ->
    tokenAnn log indent prefix ann $ do
      Log.debug log ("Formatting `SourceToken`: " <> displayShow token)
      pure (prefix <> display (Language.PureScript.CST.printToken token))

space :: Utf8Builder
space = " "

tokenAnn ::
  Log.Handle ->
  Indent ->
  Prefix ->
  Language.PureScript.CST.TokenAnn ->
  IO Utf8Builder ->
  IO Utf8Builder
tokenAnn log indent prefix tokenAnn' inside = case tokenAnn' of
  Language.PureScript.CST.TokenAnn _ leading trailing -> do
    Log.debug log "Formatting `TokenAnn`"
    commentsLeading log indent prefix leading
      <> inside
      <> commentsTrailing log prefix trailing

wrapped ::
  Log.Handle ->
  Indent ->
  (a -> IO Utf8Builder) ->
  Language.PureScript.CST.Wrapped a ->
  IO Utf8Builder
wrapped log indent f wrapped' = case wrapped' of
  Language.PureScript.CST.Wrapped open value close -> do
    let
      (before, after) = case span of
        Span.MultipleLines ->
          (space, newline <> indent)
        Span.SingleLine ->
          (blank, blank)
    Log.debug log ("Formatting `Wrapped` as " <> displayShow span <> "`")
    sourceToken log indent blank open
      <> pure before
      <> f value
      <> pure after
      <> sourceToken log indent blank close
  where
  span :: Span.Span
  span = Span.wrapped wrapped'
