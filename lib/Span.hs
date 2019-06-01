module Span
  ( Span(..)
  , betweenSourceTokens
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
  , name
  , oneOrDelimited
  , patternGuard
  , qualifiedName
  , recordLabeled
  , recordUpdate
  , separated
  , sourceToken
  , spanFromSourceRange
  , typeVarBinding
  , valueBindingFields
  , where'
  , wrapped
  ) where

import "rio" RIO

import qualified "purescript" Language.PureScript.CST
import qualified "purescript" Language.PureScript.CST.Positions
import qualified "this" SourceRange

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
delimitedNonEmpty = spanFromSourceRange . SourceRange.wrapped

doStatement :: Language.PureScript.CST.DoStatement a -> Span
doStatement =
  spanFromSourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.doStatementRange

export :: Language.PureScript.CST.Export a -> Span
export =
  spanFromSourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.exportRange

guarded :: Language.PureScript.CST.Guarded a -> Span
guarded =
  spanFromSourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.guardedRange

guardedExpr :: Language.PureScript.CST.GuardedExpr a -> Span
guardedExpr =
  spanFromSourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.guardedExprRange

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

label :: Language.PureScript.CST.Label -> Span
label =
  spanFromSourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.labelRange

labeled ::
  (a -> Language.PureScript.CST.SourceRange) ->
  (b -> Language.PureScript.CST.SourceRange) ->
  Language.PureScript.CST.Labeled a b ->
  Span
labeled f g labeled' = case labeled' of
  Language.PureScript.CST.Labeled a _ b ->
    spanFromSourceRange (Language.PureScript.CST.Positions.widen (f a) (g b))

linesBetween ::
  Language.PureScript.CST.SourcePos ->
  Language.PureScript.CST.SourcePos ->
  Int
linesBetween start end = case (start, end) of
  (Language.PureScript.CST.SourcePos line _, Language.PureScript.CST.SourcePos line' _) ->
    line - line'

name :: Language.PureScript.CST.Name a -> Span
name =
  spanFromSourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.nameRange

oneOrDelimited ::
  (a -> Language.PureScript.CST.SourceRange) ->
  Language.PureScript.CST.OneOrDelimited a ->
  Span
oneOrDelimited f = spanFromSourceRange . SourceRange.oneOrDelimited f

patternGuard :: Language.PureScript.CST.PatternGuard a -> Span
patternGuard = spanFromSourceRange . SourceRange.patternGuard

qualifiedName :: Language.PureScript.CST.QualifiedName a -> Span
qualifiedName =
  spanFromSourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.qualRange

recordLabeled ::
  (a -> Language.PureScript.CST.SourceRange) ->
  Language.PureScript.CST.RecordLabeled a ->
  Span
recordLabeled f recordLabeled' = case recordLabeled' of
  Language.PureScript.CST.RecordPun name' -> name name'
  Language.PureScript.CST.RecordField label' _ a ->
    spanFromSourceRange
      ( Language.PureScript.CST.Positions.widen
        (SourceRange.label label')
        (f a)
      )

recordUpdate :: Language.PureScript.CST.RecordUpdate a -> Span
recordUpdate = spanFromSourceRange . SourceRange.recordUpdate

separated ::
  (a -> Language.PureScript.CST.SourceRange) ->
  Language.PureScript.CST.Separated a ->
  Span
separated f = spanFromSourceRange . SourceRange.separated f

sourceToken :: Language.PureScript.CST.SourceToken -> Span
sourceToken = spanFromSourceRange . Language.PureScript.CST.Positions.srcRange

spanFromSourceRange :: Language.PureScript.CST.SourceRange -> Span
spanFromSourceRange sourceRange = case sourceRange of
  Language.PureScript.CST.SourceRange start end ->
    case linesBetween start end of
      0 -> SingleLine
      _ -> MultipleLines

typeVarBinding :: Language.PureScript.CST.TypeVarBinding a -> Span
typeVarBinding =
  spanFromSourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.typeVarBindingRange

valueBindingFields :: Language.PureScript.CST.ValueBindingFields a -> Span
valueBindingFields =
  spanFromSourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.valueBindingFieldsRange

where' :: Language.PureScript.CST.Where a -> Span
where' =
  spanFromSourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.whereRange

wrapped :: Language.PureScript.CST.Wrapped a -> Span
wrapped = spanFromSourceRange . SourceRange.wrapped
