module SourceRange
  ( adoBlock
  , binder
  , caseOf
  , constraint
  , classFundep
  , dataCtor
  , doBlock
  , export
  , expr
  , ifThenElse
  , import'
  , instance'
  , kind
  , label
  , labeled
  , lambda
  , letIn
  , name
  , oneOrDelimited
  , patternGuard
  , qualifiedName
  , recordAccessor
  , recordLabeled
  , recordUpdate
  , separated
  , type'
  , wrapped
  ) where

import "rio" RIO

import qualified "purescript-cst" Language.PureScript.CST.Positions
import qualified "purescript-cst" Language.PureScript.CST.Types
import qualified "rio" RIO.NonEmpty

adoBlock ::
  Language.PureScript.CST.Types.AdoBlock a ->
  Language.PureScript.CST.Types.SourceRange
adoBlock adoBlock' = case adoBlock' of
  Language.PureScript.CST.Types.AdoBlock ado _ _ expr' ->
    Language.PureScript.CST.Positions.widen
      (Language.PureScript.CST.Positions.srcRange ado)
      (expr expr')

binder ::
  Language.PureScript.CST.Types.Binder a ->
  Language.PureScript.CST.Types.SourceRange
binder =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.binderRange

caseOf ::
  Language.PureScript.CST.Types.CaseOf a ->
  Language.PureScript.CST.Types.SourceRange
caseOf caseOf' = case caseOf' of
  Language.PureScript.CST.Types.CaseOf case' _ _ branches ->
    Language.PureScript.CST.Positions.widen
      (Language.PureScript.CST.Positions.srcRange case')
      (guarded $ snd $ RIO.NonEmpty.last branches)

constraint ::
  Language.PureScript.CST.Types.Constraint a ->
  Language.PureScript.CST.Types.SourceRange
constraint =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.constraintRange

classFundep ::
  Language.PureScript.CST.Types.ClassFundep ->
  Language.PureScript.CST.Types.SourceRange
classFundep =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.classFundepRange

dataCtor ::
  Language.PureScript.CST.Types.DataCtor a ->
  Language.PureScript.CST.Types.SourceRange
dataCtor =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.dataCtorRange

delimitedNonEmpty ::
  Language.PureScript.CST.Types.DelimitedNonEmpty a ->
  Language.PureScript.CST.Types.SourceRange
delimitedNonEmpty = wrapped

doBlock ::
  Language.PureScript.CST.Types.DoBlock a ->
  Language.PureScript.CST.Types.SourceRange
doBlock doBlock' = case doBlock' of
  Language.PureScript.CST.Types.DoBlock do' doStatements ->
    Language.PureScript.CST.Positions.widen
      (Language.PureScript.CST.Positions.srcRange do')
      (doStatement $ RIO.NonEmpty.last doStatements)

doStatement ::
  Language.PureScript.CST.Types.DoStatement a ->
  Language.PureScript.CST.Types.SourceRange
doStatement =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.doStatementRange

expr ::
  Language.PureScript.CST.Types.Expr a ->
  Language.PureScript.CST.Types.SourceRange
expr =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.exprRange

export ::
  Language.PureScript.CST.Types.Export a ->
  Language.PureScript.CST.Types.SourceRange
export =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.exportRange

guarded ::
  Language.PureScript.CST.Types.Guarded a ->
  Language.PureScript.CST.Types.SourceRange
guarded =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.guardedRange

ifThenElse ::
  Language.PureScript.CST.Types.IfThenElse a ->
  Language.PureScript.CST.Types.SourceRange
ifThenElse ifThenElse' = case ifThenElse' of
  Language.PureScript.CST.Types.IfThenElse if' _ _ _ _ expr' ->
    Language.PureScript.CST.Positions.widen
      (Language.PureScript.CST.Positions.srcRange if')
      (expr expr')

import' ::
  Language.PureScript.CST.Types.Import a ->
  Language.PureScript.CST.Types.SourceRange
import' =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.importRange

instance' ::
  Language.PureScript.CST.Types.Instance a ->
  Language.PureScript.CST.Types.SourceRange
instance' =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.instanceRange

kind ::
  Language.PureScript.CST.Types.Kind a ->
  Language.PureScript.CST.Types.SourceRange
kind =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.kindRange

label ::
  Language.PureScript.CST.Types.Label ->
  Language.PureScript.CST.Types.SourceRange
label =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.labelRange

labeled ::
  (a -> Language.PureScript.CST.Types.SourceRange) ->
  (b -> Language.PureScript.CST.Types.SourceRange) ->
  Language.PureScript.CST.Types.Labeled a b ->
  Language.PureScript.CST.Types.SourceRange
labeled f g labeled' = case labeled' of
  Language.PureScript.CST.Types.Labeled label' _ value ->
    Language.PureScript.CST.Positions.widen (f label') (g value)

lambda ::
  Language.PureScript.CST.Types.Lambda a ->
  Language.PureScript.CST.Types.SourceRange
lambda lambda' = case lambda' of
  Language.PureScript.CST.Types.Lambda reverseSolidus _ _ expr' ->
    Language.PureScript.CST.Positions.widen
      (Language.PureScript.CST.Positions.srcRange reverseSolidus)
      (expr expr')

letIn ::
  Language.PureScript.CST.Types.LetIn a ->
  Language.PureScript.CST.Types.SourceRange
letIn letIn' = case letIn' of
  Language.PureScript.CST.Types.LetIn let' _ _ expr' ->
    Language.PureScript.CST.Positions.widen
      (Language.PureScript.CST.Positions.srcRange let')
      (expr expr')

name ::
  Language.PureScript.CST.Types.Name a ->
  Language.PureScript.CST.Types.SourceRange
name =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.nameRange

oneOrDelimited ::
  (a -> Language.PureScript.CST.Types.SourceRange) ->
  Language.PureScript.CST.Types.OneOrDelimited a ->
  Language.PureScript.CST.Types.SourceRange
oneOrDelimited f oneOrDelimited' = case oneOrDelimited' of
  Language.PureScript.CST.Types.One a -> f a
  Language.PureScript.CST.Types.Many delimitedNonEmpty' ->
    delimitedNonEmpty delimitedNonEmpty'

patternGuard ::
  Language.PureScript.CST.Types.PatternGuard a ->
  Language.PureScript.CST.Types.SourceRange
patternGuard patternGuard' = case patternGuard' of
  Language.PureScript.CST.Types.PatternGuard binder' expr' ->
    Language.PureScript.CST.Positions.widen
      (maybe (expr expr') (binder . fst) binder')
      (expr expr')

qualifiedName ::
  Language.PureScript.CST.Types.QualifiedName a ->
  Language.PureScript.CST.Types.SourceRange
qualifiedName =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.qualRange

recordAccessor ::
  Language.PureScript.CST.Types.RecordAccessor a ->
  Language.PureScript.CST.Types.SourceRange
recordAccessor recordAccessor' = case recordAccessor' of
  Language.PureScript.CST.Types.RecordAccessor expr' _ path ->
    Language.PureScript.CST.Positions.widen (expr expr') (separated label path)

recordLabeled ::
  (a -> Language.PureScript.CST.Types.SourceRange) ->
  Language.PureScript.CST.Types.RecordLabeled a ->
  Language.PureScript.CST.Types.SourceRange
recordLabeled f recordLabeled' = case recordLabeled' of
  Language.PureScript.CST.Types.RecordPun name' -> name name'
  Language.PureScript.CST.Types.RecordField label' _ a ->
    Language.PureScript.CST.Positions.widen (label label') (f a)

recordUpdate ::
  Language.PureScript.CST.Types.RecordUpdate a ->
  Language.PureScript.CST.Types.SourceRange
recordUpdate recordUpdate' = case recordUpdate' of
  Language.PureScript.CST.Types.RecordUpdateBranch label' delimitedNonEmpty' ->
    Language.PureScript.CST.Positions.widen
      (label label')
      (delimitedNonEmpty delimitedNonEmpty')
  Language.PureScript.CST.Types.RecordUpdateLeaf label' _ expr' ->
    Language.PureScript.CST.Positions.widen (label label') (expr expr')

separated ::
  (a -> Language.PureScript.CST.Types.SourceRange) ->
  Language.PureScript.CST.Types.Separated a ->
  Language.PureScript.CST.Types.SourceRange
separated f separated' = case separated' of
  Language.PureScript.CST.Types.Separated head _ ->
    Language.PureScript.CST.Positions.widen
      (f head)
      (f $ Language.PureScript.CST.Positions.sepLast separated')

type' ::
  Language.PureScript.CST.Types.Type a ->
  Language.PureScript.CST.Types.SourceRange
type' =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.typeRange

wrapped ::
  Language.PureScript.CST.Types.Wrapped a ->
  Language.PureScript.CST.Types.SourceRange
wrapped =
  Language.PureScript.CST.Positions.toSourceRange
    . Language.PureScript.CST.Positions.wrappedRange
