{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Span
  ( Span (..),
    adoBlock,
    betweenSourceRanges,
    betweenSourceTokens,
    binder,
    caseOf,
    comment,
    constraint,
    dataCtor,
    dataMembers,
    declaration,
    delimitedNonEmpty,
    doBlock,
    doStatement,
    export,
    expr,
    foreign',
    guarded,
    guardedExpr,
    ifThenElse,
    import',
    importDecl,
    instance',
    instanceBinding,
    instanceHead,
    label,
    labeled,
    lambda,
    lineFeed,
    letBinding,
    letIn,
    name,
    oneOrDelimited,
    patternGuard,
    qualifiedName,
    recordAccessor,
    recordLabeled,
    recordUpdate,
    row,
    separated,
    sourceRange,
    sourceToken,
    type',
    typeVarBinding,
    valueBindingFields,
    where',
    wrapped,
  )
where

import qualified "purescript-cst" Language.PureScript.CST.Positions
import qualified "purescript-cst" Language.PureScript.CST.Types
import "rio" RIO
import qualified "rio" RIO.Text
import qualified "purs-tool-cst" SourceRange

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

adoBlock :: Language.PureScript.CST.Types.AdoBlock a -> Span
adoBlock = sourceRange . SourceRange.adoBlock

betweenSourceRanges ::
  Language.PureScript.CST.Types.SourceRange ->
  Language.PureScript.CST.Types.SourceRange ->
  Span
betweenSourceRanges start end =
  sourceRange (Language.PureScript.CST.Positions.widen start end)

betweenSourceTokens ::
  Language.PureScript.CST.Types.SourceToken ->
  Language.PureScript.CST.Types.SourceToken ->
  Span
betweenSourceTokens start end =
  sourceRange (Language.PureScript.CST.Positions.toSourceRange (start, end))

binder :: Language.PureScript.CST.Types.Binder a -> Span
binder =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.binderRange

caseOf :: Language.PureScript.CST.Types.CaseOf a -> Span
caseOf = sourceRange . SourceRange.caseOf

comment :: (a -> Span) -> Language.PureScript.CST.Types.Comment a -> Span
comment f comment'' = case comment'' of
  Language.PureScript.CST.Types.Comment comment' ->
    case RIO.Text.find (== '\n') comment' of
      Nothing -> Span.SingleLine
      Just _ -> Span.MultipleLines
  Language.PureScript.CST.Types.Line a -> f a
  Language.PureScript.CST.Types.Space _ -> Span.SingleLine

constraint :: Language.PureScript.CST.Types.Constraint a -> Span
constraint =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.constraintRange

dataCtor :: Language.PureScript.CST.Types.DataCtor a -> Span
dataCtor =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.dataCtorRange

dataMembers :: Language.PureScript.CST.Types.DataMembers a -> Span
dataMembers =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.dataMembersRange

declaration :: Language.PureScript.CST.Types.Declaration a -> Span
declaration =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.declRange

delimitedNonEmpty :: Language.PureScript.CST.Types.DelimitedNonEmpty a -> Span
delimitedNonEmpty = sourceRange . SourceRange.wrapped

doBlock :: Language.PureScript.CST.Types.DoBlock a -> Span
doBlock = sourceRange . SourceRange.doBlock

doStatement :: Language.PureScript.CST.Types.DoStatement a -> Span
doStatement =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.doStatementRange

export :: Language.PureScript.CST.Types.Export a -> Span
export =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.exportRange

expr :: Language.PureScript.CST.Types.Expr a -> Span
expr =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.exprRange

foreign' :: Language.PureScript.CST.Types.Foreign a -> Span
foreign' =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.foreignRange

guarded :: Language.PureScript.CST.Types.Guarded a -> Span
guarded =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.guardedRange

guardedExpr :: Language.PureScript.CST.Types.GuardedExpr a -> Span
guardedExpr =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.guardedExprRange

ifThenElse :: Language.PureScript.CST.Types.IfThenElse a -> Span
ifThenElse = sourceRange . SourceRange.ifThenElse

import' :: Language.PureScript.CST.Types.Import a -> Span
import' =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.importRange

importDecl :: Language.PureScript.CST.Types.ImportDecl a -> Span
importDecl =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.importDeclRange

instance' :: Language.PureScript.CST.Types.Instance a -> Span
instance' =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.instanceRange

instanceBinding :: Language.PureScript.CST.Types.InstanceBinding a -> Span
instanceBinding =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.instanceBindingRange

instanceHead :: Language.PureScript.CST.Types.InstanceHead a -> Span
instanceHead =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.instanceHeadRange

label :: Language.PureScript.CST.Types.Label -> Span
label =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.labelRange

labeled ::
  (a -> Language.PureScript.CST.Types.SourceRange) ->
  (b -> Language.PureScript.CST.Types.SourceRange) ->
  Language.PureScript.CST.Types.Labeled a b ->
  Span
labeled f g labeled' = case labeled' of
  Language.PureScript.CST.Types.Labeled a _ b ->
    sourceRange (Language.PureScript.CST.Positions.widen (f a) (g b))

lambda :: Language.PureScript.CST.Types.Lambda a -> Span
lambda = sourceRange . SourceRange.lambda

linesBetween ::
  Language.PureScript.CST.Types.SourcePos ->
  Language.PureScript.CST.Types.SourcePos ->
  Int
linesBetween start end = case (start, end) of
  (Language.PureScript.CST.Types.SourcePos line _, Language.PureScript.CST.Types.SourcePos line' _) ->
    line - line'

lineFeed :: Language.PureScript.CST.Types.LineFeed -> Span
lineFeed lineFeed' = case lineFeed' of
  Language.PureScript.CST.Types.LF -> Span.SingleLine
  Language.PureScript.CST.Types.CRLF -> Span.SingleLine

letBinding :: Language.PureScript.CST.Types.LetBinding a -> Span
letBinding =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.letBindingRange

letIn :: Language.PureScript.CST.Types.LetIn a -> Span
letIn = sourceRange . SourceRange.letIn

name :: Language.PureScript.CST.Types.Name a -> Span
name =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.nameRange

oneOrDelimited ::
  (a -> Language.PureScript.CST.Types.SourceRange) ->
  Language.PureScript.CST.Types.OneOrDelimited a ->
  Span
oneOrDelimited f = sourceRange . SourceRange.oneOrDelimited f

patternGuard :: Language.PureScript.CST.Types.PatternGuard a -> Span
patternGuard = sourceRange . SourceRange.patternGuard

qualifiedName :: Language.PureScript.CST.Types.QualifiedName a -> Span
qualifiedName =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.qualRange

recordAccessor :: Language.PureScript.CST.Types.RecordAccessor a -> Span
recordAccessor = sourceRange . SourceRange.recordAccessor

recordLabeled ::
  (a -> Language.PureScript.CST.Types.SourceRange) ->
  Language.PureScript.CST.Types.RecordLabeled a ->
  Span
recordLabeled f recordLabeled' = case recordLabeled' of
  Language.PureScript.CST.Types.RecordPun name' -> name name'
  Language.PureScript.CST.Types.RecordField label' _ a ->
    sourceRange
      ( Language.PureScript.CST.Positions.widen
          (SourceRange.label label')
          (f a)
      )

recordUpdate :: Language.PureScript.CST.Types.RecordUpdate a -> Span
recordUpdate = sourceRange . SourceRange.recordUpdate

row :: Language.PureScript.CST.Types.Row a -> Span
row row' = case row' of
  Language.PureScript.CST.Types.Row labels' tail -> case (labels', tail) of
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
  (a -> Language.PureScript.CST.Types.SourceRange) ->
  Language.PureScript.CST.Types.Separated a ->
  Span
separated f = sourceRange . SourceRange.separated f

sourceToken :: Language.PureScript.CST.Types.SourceToken -> Span
sourceToken = sourceRange . Language.PureScript.CST.Positions.srcRange

sourceRange :: Language.PureScript.CST.Types.SourceRange -> Span
sourceRange sourceRange' = case sourceRange' of
  Language.PureScript.CST.Types.SourceRange start end ->
    case linesBetween start end of
      0 -> SingleLine
      _ -> MultipleLines

type' :: Language.PureScript.CST.Types.Type a -> Span
type' =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.typeRange

typeVarBinding :: Language.PureScript.CST.Types.TypeVarBinding a -> Span
typeVarBinding =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.typeVarBindingRange

valueBindingFields :: Language.PureScript.CST.Types.ValueBindingFields a -> Span
valueBindingFields =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.valueBindingFieldsRange

where' :: Language.PureScript.CST.Types.Where a -> Span
where' =
  sourceRange
    . Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.whereRange

wrapped :: Language.PureScript.CST.Types.Wrapped a -> Span
wrapped = sourceRange . SourceRange.wrapped
