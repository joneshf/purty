module Span
  ( Span(..)
  , adoBlock
  , betweenSourceRanges
  , betweenSourceTokens
  , binder
  , caseOf
  , comment
  , constraint
  , dataCtor
  , dataMembers
  , declaration
  , delimitedNonEmpty
  , doBlock
  , doStatement
  , export
  , expr
  , foreign'
  , guarded
  , guardedExpr
  , ifThenElse
  , import'
  , importDecl
  , instance'
  , instanceBinding
  , instanceHead
  , kind
  , label
  , labeled
  , lambda
  , lineFeed
  , letBinding
  , letIn
  , name
  , oneOrDelimited
  , patternGuard
  , qualifiedName
  , recordAccessor
  , recordLabeled
  , recordUpdate
  , row
  , separated
  , sourceRange
  , sourceToken
  , type'
  , typeVarBinding
  , valueBindingFields
  , where'
  , wrapped
  ) where

import "rio" RIO

import qualified "text" Data.Text
import qualified "purescript" Language.PureScript.CST
import qualified "purescript" Language.PureScript.CST.Positions
import qualified "this" SourceRange

data Span
  = MultipleLines
  | SingleLine
  deriving (Show)

instance Semigroup Span where
  span1 <> span2 = case (span1, span2) of
    (MultipleLines, MultipleLines) -> MultipleLines
    (MultipleLines, SingleLine) -> MultipleLines
    (SingleLine, MultipleLines) -> MultipleLines
    (SingleLine, SingleLine) -> SingleLine

instance Monoid Span where
  mempty = SingleLine

adoBlock :: Language.PureScript.CST.AdoBlock a -> Span
adoBlock = sourceRange . SourceRange.adoBlock

betweenSourceRanges ::
  Language.PureScript.CST.SourceRange ->
  Language.PureScript.CST.SourceRange ->
  Span
betweenSourceRanges start end =
  sourceRange (Language.PureScript.CST.Positions.widen start end)

betweenSourceTokens ::
  Language.PureScript.CST.SourceToken ->
  Language.PureScript.CST.SourceToken ->
  Span
betweenSourceTokens start end =
  sourceRange (Language.PureScript.CST.Positions.toSourceRange (start, end))

binder :: Language.PureScript.CST.Binder a -> Span
binder =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.binderRange

caseOf :: Language.PureScript.CST.CaseOf a -> Span
caseOf = sourceRange . SourceRange.caseOf

comment :: (a -> Span) -> Language.PureScript.CST.Comment a -> Span
comment f comment'' = case comment'' of
  Language.PureScript.CST.Comment comment' ->
    case Data.Text.count "\n" comment' of
      0 -> Span.SingleLine
      _ -> Span.MultipleLines
  Language.PureScript.CST.Line a -> f a
  Language.PureScript.CST.Space _ -> Span.SingleLine

constraint :: Language.PureScript.CST.Constraint a -> Span
constraint =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.constraintRange

dataCtor :: Language.PureScript.CST.DataCtor a -> Span
dataCtor =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.dataCtorRange

dataMembers :: Language.PureScript.CST.DataMembers a -> Span
dataMembers =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.dataMembersRange

declaration :: Language.PureScript.CST.Declaration a -> Span
declaration =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.declRange

delimitedNonEmpty :: Language.PureScript.CST.DelimitedNonEmpty a -> Span
delimitedNonEmpty = sourceRange . SourceRange.wrapped

doBlock :: Language.PureScript.CST.DoBlock a -> Span
doBlock = sourceRange . SourceRange.doBlock

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

expr :: Language.PureScript.CST.Expr a -> Span
expr =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.exprRange

foreign' :: Language.PureScript.CST.Foreign a -> Span
foreign' =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.foreignRange

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

ifThenElse :: Language.PureScript.CST.IfThenElse a -> Span
ifThenElse = sourceRange . SourceRange.ifThenElse

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

instance' :: Language.PureScript.CST.Instance a -> Span
instance' =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.instanceRange

instanceBinding :: Language.PureScript.CST.InstanceBinding a -> Span
instanceBinding =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.instanceBindingRange

instanceHead :: Language.PureScript.CST.InstanceHead a -> Span
instanceHead =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.instanceHeadRange

kind :: Language.PureScript.CST.Kind a -> Span
kind =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.kindRange

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

lambda :: Language.PureScript.CST.Lambda a -> Span
lambda = sourceRange . SourceRange.lambda

linesBetween ::
  Language.PureScript.CST.SourcePos ->
  Language.PureScript.CST.SourcePos ->
  Int
linesBetween start end = case (start, end) of
  (Language.PureScript.CST.SourcePos line _, Language.PureScript.CST.SourcePos line' _) ->
    line - line'

lineFeed :: Language.PureScript.CST.LineFeed -> Span
lineFeed lineFeed' = case lineFeed' of
  Language.PureScript.CST.LF   -> Span.SingleLine
  Language.PureScript.CST.CRLF -> Span.SingleLine

letBinding :: Language.PureScript.CST.LetBinding a -> Span
letBinding =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.letBindingRange

letIn :: Language.PureScript.CST.LetIn a -> Span
letIn = sourceRange . SourceRange.letIn

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

recordAccessor :: Language.PureScript.CST.RecordAccessor a -> Span
recordAccessor = sourceRange . SourceRange.recordAccessor

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

row :: Language.PureScript.CST.Row a -> Span
row row' = case row' of
  Language.PureScript.CST.Row labels' tail -> case (labels', tail) of
    (Just labels, Just (_, type'')) ->
      sourceRange
        ( Language.PureScript.CST.Positions.widen
          ( SourceRange.separated
            (SourceRange.labeled SourceRange.label SourceRange.type')
            labels
          )
          (SourceRange.type' type'')
        )
    (Just labels, Nothing) ->
      separated (SourceRange.labeled SourceRange.label SourceRange.type') labels
    (Nothing, Just (_, type'')) -> type' type''
    (Nothing, Nothing) -> SingleLine

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

type' :: Language.PureScript.CST.Type a -> Span
type' =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.typeRange

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
