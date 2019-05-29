module Span
  ( Span(..)
  , betweenSourceTokens
  , dataMembers
  , delimitedNonEmpty
  , export
  , import'
  , importDecl
  , separated
  , sourceRangeFromExport
  , sourceRangeFromImport
  , sourceRangeFromName
  , wrapped
  ) where

import "rio" RIO

import qualified "purescript" Language.PureScript.CST
import qualified "purescript" Language.PureScript.CST.Positions

data Span
  = MultipleLines
  | SingleLine
  deriving (Show)

betweenSourceTokens ::
  Language.PureScript.CST.SourceToken ->
  Language.PureScript.CST.SourceToken ->
  Span
betweenSourceTokens start end =
  spanFromSourceRange
    (Language.PureScript.CST.Positions.toSourceRange (start, end))

dataMembers :: Language.PureScript.CST.DataMembers a -> Span
dataMembers =
  spanFromSourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.dataMembersRange

delimitedNonEmpty :: Language.PureScript.CST.DelimitedNonEmpty a -> Span
delimitedNonEmpty = spanFromSourceRange . sourceRangeFromWrapped

export :: Language.PureScript.CST.Export a -> Span
export =
  spanFromSourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.exportRange

import' :: Language.PureScript.CST.Import a -> Span
import' =
  spanFromSourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.importRange

importDecl :: Language.PureScript.CST.ImportDecl a -> Span
importDecl =
  spanFromSourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.importDeclRange

linesBetween ::
  Language.PureScript.CST.SourcePos ->
  Language.PureScript.CST.SourcePos ->
  Int
linesBetween start end = case (start, end) of
  (Language.PureScript.CST.SourcePos line _, Language.PureScript.CST.SourcePos line' _) ->
    line - line'

separated ::
  (a -> Language.PureScript.CST.SourceRange) ->
  Language.PureScript.CST.Separated a ->
  Span
separated f = spanFromSourceRange . sourceRangeFromSeparated f

sourceRangeFromExport ::
  Language.PureScript.CST.Export a ->
  Language.PureScript.CST.SourceRange
sourceRangeFromExport =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.exportRange

sourceRangeFromImport ::
  Language.PureScript.CST.Import a ->
  Language.PureScript.CST.SourceRange
sourceRangeFromImport =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.importRange

sourceRangeFromName ::
  Language.PureScript.CST.Name a ->
  Language.PureScript.CST.SourceRange
sourceRangeFromName =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.nameRange

sourceRangeFromSeparated ::
  (a -> Language.PureScript.CST.SourceRange) ->
  Language.PureScript.CST.Separated a ->
  Language.PureScript.CST.SourceRange
sourceRangeFromSeparated f separated' = case separated' of
  Language.PureScript.CST.Separated head _ ->
    Language.PureScript.CST.Positions.widen
      (f head)
      (f $ Language.PureScript.CST.Positions.sepLast separated')

sourceRangeFromWrapped ::
  Language.PureScript.CST.Wrapped a ->
  Language.PureScript.CST.SourceRange
sourceRangeFromWrapped =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.wrappedRange

spanFromSourceRange :: Language.PureScript.CST.SourceRange -> Span
spanFromSourceRange sourceRange = case sourceRange of
  Language.PureScript.CST.SourceRange start end ->
    case linesBetween start end of
      0 -> SingleLine
      _ -> MultipleLines

wrapped :: Language.PureScript.CST.Wrapped a -> Span
wrapped = spanFromSourceRange . sourceRangeFromWrapped
