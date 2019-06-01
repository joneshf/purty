module SourceRange
  ( binder
  , constraint
  , classFundep
  , dataCtor
  , export
  , expr
  , import'
  , instance'
  , kind
  , label
  , labeled
  , name
  , oneOrDelimited
  , patternGuard
  , recordLabeled
  , recordUpdate
  , separated
  , type'
  , wrapped
  ) where

import "rio" RIO

import qualified "purescript" Language.PureScript.CST
import qualified "purescript" Language.PureScript.CST.Positions

binder ::
  Language.PureScript.CST.Binder a ->
  Language.PureScript.CST.SourceRange
binder =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.binderRange

constraint ::
  Language.PureScript.CST.Constraint a ->
  Language.PureScript.CST.SourceRange
constraint =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.constraintRange

classFundep ::
  Language.PureScript.CST.ClassFundep ->
  Language.PureScript.CST.SourceRange
classFundep =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.classFundepRange

dataCtor ::
  Language.PureScript.CST.DataCtor a ->
  Language.PureScript.CST.SourceRange
dataCtor =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.dataCtorRange

delimitedNonEmpty ::
  Language.PureScript.CST.DelimitedNonEmpty a ->
  Language.PureScript.CST.SourceRange
delimitedNonEmpty = wrapped

expr ::
  Language.PureScript.CST.Expr a ->
  Language.PureScript.CST.SourceRange
expr =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.exprRange

export ::
  Language.PureScript.CST.Export a ->
  Language.PureScript.CST.SourceRange
export =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.exportRange

import' ::
  Language.PureScript.CST.Import a ->
  Language.PureScript.CST.SourceRange
import' =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.importRange

instance' ::
  Language.PureScript.CST.Instance a ->
  Language.PureScript.CST.SourceRange
instance' =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.instanceRange

kind ::
  Language.PureScript.CST.Kind a ->
  Language.PureScript.CST.SourceRange
kind =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.kindRange

label ::
  Language.PureScript.CST.Label ->
  Language.PureScript.CST.SourceRange
label =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.labelRange

labeled ::
  (a -> Language.PureScript.CST.SourceRange) ->
  (b -> Language.PureScript.CST.SourceRange) ->
  Language.PureScript.CST.Labeled a b ->
  Language.PureScript.CST.SourceRange
labeled f g labeled' = case labeled' of
  Language.PureScript.CST.Labeled label' _ value ->
    Language.PureScript.CST.Positions.widen (f label') (g value)

name ::
  Language.PureScript.CST.Name a ->
  Language.PureScript.CST.SourceRange
name =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.nameRange

oneOrDelimited ::
  (a -> Language.PureScript.CST.SourceRange) ->
  Language.PureScript.CST.OneOrDelimited a ->
  Language.PureScript.CST.SourceRange
oneOrDelimited f oneOrDelimited' = case oneOrDelimited' of
  Language.PureScript.CST.One a -> f a
  Language.PureScript.CST.Many delimitedNonEmpty' ->
    delimitedNonEmpty delimitedNonEmpty'

patternGuard ::
  Language.PureScript.CST.PatternGuard a ->
  Language.PureScript.CST.SourceRange
patternGuard patternGuard' = case patternGuard' of
  Language.PureScript.CST.PatternGuard binder' expr' ->
    Language.PureScript.CST.Positions.widen
      (maybe (expr expr') (binder . fst) binder')
      (expr expr')

recordLabeled ::
  (a -> Language.PureScript.CST.SourceRange) ->
  Language.PureScript.CST.RecordLabeled a ->
  Language.PureScript.CST.SourceRange
recordLabeled f recordLabeled' = case recordLabeled' of
  Language.PureScript.CST.RecordPun name' -> name name'
  Language.PureScript.CST.RecordField label' _ a ->
    Language.PureScript.CST.Positions.widen (label label') (f a)

recordUpdate ::
  Language.PureScript.CST.RecordUpdate a ->
  Language.PureScript.CST.SourceRange
recordUpdate recordUpdate' = case recordUpdate' of
  Language.PureScript.CST.RecordUpdateBranch label' delimitedNonEmpty' ->
    Language.PureScript.CST.Positions.widen
      (label label')
      (delimitedNonEmpty delimitedNonEmpty')
  Language.PureScript.CST.RecordUpdateLeaf label' _ expr' ->
    Language.PureScript.CST.Positions.widen (label label') (expr expr')

separated ::
  (a -> Language.PureScript.CST.SourceRange) ->
  Language.PureScript.CST.Separated a ->
  Language.PureScript.CST.SourceRange
separated f separated' = case separated' of
  Language.PureScript.CST.Separated head _ ->
    Language.PureScript.CST.Positions.widen
      (f head)
      (f $ Language.PureScript.CST.Positions.sepLast separated')

type' ::
  Language.PureScript.CST.Type a ->
  Language.PureScript.CST.SourceRange
type' =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.typeRange

wrapped ::
  Language.PureScript.CST.Wrapped a ->
  Language.PureScript.CST.SourceRange
wrapped =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.wrappedRange
