module Span
  ( Span(..)
  , betweenSourceTokens
  , comment
  , dataMembers
  , delimitedNonEmpty
  , doStatement
  , export
  , guarded
  , guardedExpr
  , import'
  , importDecl
  , label
  , labeled
  , lineFeed
  , name
  , oneOrDelimited
  , patternGuard
  , qualifiedName
  , recordLabeled
  , recordUpdate
  , separated
  , sourceRange
  , sourceToken
  , typeVarBinding
  , valueBindingFields
  , where'
  , wrapped
  ) where

import "rio" RIO

import qualified "purescript" Language.PureScript.CST
import qualified "purescript" Language.PureScript.CST.Positions
import qualified "this" SourceRange
import qualified "text" Data.Text

data Span
  = MultipleLines
  | SingleLine
  deriving (Show)

betweenSourceTokens ::
  Language.PureScript.CST.SourceToken ->
  Language.PureScript.CST.SourceToken ->
  Span
betweenSourceTokens start end =
  sourceRange
    (Language.PureScript.CST.Positions.toSourceRange (start, end))

comment :: (a -> Span) -> Language.PureScript.CST.Comment a -> Span
comment f comment'' = case comment'' of
  Language.PureScript.CST.Comment comment' ->
    case Data.Text.count "\n" comment' of
      0 -> Span.SingleLine
      _ -> Span.MultipleLines
  Language.PureScript.CST.Line a -> f a
  Language.PureScript.CST.Space _ -> Span.SingleLine

dataMembers :: Language.PureScript.CST.DataMembers a -> Span
dataMembers =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.dataMembersRange

delimitedNonEmpty :: Language.PureScript.CST.DelimitedNonEmpty a -> Span
delimitedNonEmpty = sourceRange . SourceRange.wrapped

doStatement :: Language.PureScript.CST.DoStatement a -> Span
doStatement =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.doStatementRange

export :: Language.PureScript.CST.Export a -> Span
export =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.exportRange

guarded :: Language.PureScript.CST.Guarded a -> Span
guarded =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.guardedRange

guardedExpr :: Language.PureScript.CST.GuardedExpr a -> Span
guardedExpr =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.guardedExprRange

import' :: Language.PureScript.CST.Import a -> Span
import' =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.importRange

importDecl :: Language.PureScript.CST.ImportDecl a -> Span
importDecl =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.importDeclRange

label :: Language.PureScript.CST.Label -> Span
label =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.labelRange

labeled ::
  (a -> Language.PureScript.CST.SourceRange) ->
  (b -> Language.PureScript.CST.SourceRange) ->
  Language.PureScript.CST.Labeled a b ->
  Span
labeled f g labeled' = case labeled' of
  Language.PureScript.CST.Labeled a _ b ->
    sourceRange (Language.PureScript.CST.Positions.widen (f a) (g b))

linesBetween ::
  Language.PureScript.CST.SourcePos ->
  Language.PureScript.CST.SourcePos ->
  Int
linesBetween start end = case (start, end) of
  (Language.PureScript.CST.SourcePos line _, Language.PureScript.CST.SourcePos line' _) ->
    line - line'

lineFeed :: Language.PureScript.CST.LineFeed -> Span
lineFeed lineFeed' = case lineFeed' of
  Language.PureScript.CST.LF -> Span.SingleLine
  Language.PureScript.CST.CRLF -> Span.SingleLine

name :: Language.PureScript.CST.Name a -> Span
name =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.nameRange

oneOrDelimited ::
  (a -> Language.PureScript.CST.SourceRange) ->
  Language.PureScript.CST.OneOrDelimited a ->
  Span
oneOrDelimited f = sourceRange . SourceRange.oneOrDelimited f

patternGuard :: Language.PureScript.CST.PatternGuard a -> Span
patternGuard = sourceRange . SourceRange.patternGuard

qualifiedName :: Language.PureScript.CST.QualifiedName a -> Span
qualifiedName =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.qualRange

recordLabeled ::
  (a -> Language.PureScript.CST.SourceRange) ->
  Language.PureScript.CST.RecordLabeled a ->
  Span
recordLabeled f recordLabeled' = case recordLabeled' of
  Language.PureScript.CST.RecordPun name' -> name name'
  Language.PureScript.CST.RecordField label' _ a ->
    sourceRange
      ( Language.PureScript.CST.Positions.widen
        (SourceRange.label label')
        (f a)
      )

recordUpdate :: Language.PureScript.CST.RecordUpdate a -> Span
recordUpdate = sourceRange . SourceRange.recordUpdate

separated ::
  (a -> Language.PureScript.CST.SourceRange) ->
  Language.PureScript.CST.Separated a ->
  Span
separated f = sourceRange . SourceRange.separated f

sourceToken :: Language.PureScript.CST.SourceToken -> Span
sourceToken = sourceRange . Language.PureScript.CST.Positions.srcRange

sourceRange :: Language.PureScript.CST.SourceRange -> Span
sourceRange sourceRange' = case sourceRange' of
  Language.PureScript.CST.SourceRange start end ->
    case linesBetween start end of
      0 -> SingleLine
      _ -> MultipleLines

typeVarBinding :: Language.PureScript.CST.TypeVarBinding a -> Span
typeVarBinding =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.typeVarBindingRange

valueBindingFields :: Language.PureScript.CST.ValueBindingFields a -> Span
valueBindingFields =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.valueBindingFieldsRange

where' :: Language.PureScript.CST.Where a -> Span
where' =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.whereRange

wrapped :: Language.PureScript.CST.Wrapped a -> Span
wrapped = sourceRange . SourceRange.wrapped
