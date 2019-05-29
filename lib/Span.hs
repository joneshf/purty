module Span
  ( Span(..)
  , delimitedNonEmpty
  , separated
  , sourceRangeFromExport
  , sourceRangeFromImport
  , sourceRangeFromName
  , wrapped
  ) where

import "rio" RIO

import qualified "purescript" Language.PureScript.CST
import qualified "rio" RIO.List

data Span
  = MultipleLines
  | SingleLine
  deriving (Show)

delimitedNonEmpty :: Language.PureScript.CST.DelimitedNonEmpty a -> Span
delimitedNonEmpty = spanFromSourceRange . sourceRangeFromWrapped

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

sourceRangeFromDataMembers ::
  Language.PureScript.CST.DataMembers a ->
  Language.PureScript.CST.SourceRange
sourceRangeFromDataMembers dataMembers' = case dataMembers' of
  Language.PureScript.CST.DataAll _ sourceToken' ->
    sourceRangeFromSourceToken sourceToken'
  Language.PureScript.CST.DataEnumerated _ delimited' ->
    sourceRangeFromDelimited delimited'

sourceRangeFromDelimited ::
  Language.PureScript.CST.Delimited a ->
  Language.PureScript.CST.SourceRange
sourceRangeFromDelimited = sourceRangeFromWrapped

sourceRangeFromExport ::
  Language.PureScript.CST.Export a ->
  Language.PureScript.CST.SourceRange
sourceRangeFromExport export' = case export' of
  Language.PureScript.CST.ExportClass _ class' name' ->
    Language.PureScript.CST.SourceRange
      { Language.PureScript.CST.srcStart =
        Language.PureScript.CST.srcStart (sourceRangeFromSourceToken class')
      , Language.PureScript.CST.srcEnd =
        Language.PureScript.CST.srcEnd (sourceRangeFromName name')
      }
  Language.PureScript.CST.ExportKind _ kind' name' ->
    Language.PureScript.CST.SourceRange
      { Language.PureScript.CST.srcStart =
        Language.PureScript.CST.srcStart (sourceRangeFromSourceToken kind')
      , Language.PureScript.CST.srcEnd =
        Language.PureScript.CST.srcEnd (sourceRangeFromName name')
      }
  Language.PureScript.CST.ExportModule _ module'' name' ->
    Language.PureScript.CST.SourceRange
      { Language.PureScript.CST.srcStart =
        Language.PureScript.CST.srcStart (sourceRangeFromSourceToken module'')
      , Language.PureScript.CST.srcEnd =
        Language.PureScript.CST.srcEnd (sourceRangeFromName name')
      }
  Language.PureScript.CST.ExportOp _ name' -> sourceRangeFromName name'
  Language.PureScript.CST.ExportType _ name' dataMembers' ->
    Language.PureScript.CST.SourceRange
      { Language.PureScript.CST.srcStart =
        Language.PureScript.CST.srcStart (sourceRangeFromName name')
      , Language.PureScript.CST.srcEnd =
        Language.PureScript.CST.srcEnd
          ( maybe
            (sourceRangeFromName name')
            sourceRangeFromDataMembers
            dataMembers'
          )
      }
  Language.PureScript.CST.ExportTypeOp _ type'' name' ->
    Language.PureScript.CST.SourceRange
      { Language.PureScript.CST.srcStart =
        Language.PureScript.CST.srcStart (sourceRangeFromSourceToken type'')
      , Language.PureScript.CST.srcEnd =
        Language.PureScript.CST.srcEnd (sourceRangeFromName name')
      }
  Language.PureScript.CST.ExportValue _ name' -> sourceRangeFromName name'

sourceRangeFromImport ::
  Language.PureScript.CST.Import a ->
  Language.PureScript.CST.SourceRange
sourceRangeFromImport import' = case import' of
  Language.PureScript.CST.ImportClass _ class' name' ->
    Language.PureScript.CST.SourceRange
      { Language.PureScript.CST.srcStart =
        Language.PureScript.CST.srcStart (sourceRangeFromSourceToken class')
      , Language.PureScript.CST.srcEnd =
        Language.PureScript.CST.srcEnd (sourceRangeFromName name')
      }
  Language.PureScript.CST.ImportKind _ kind' name' ->
    Language.PureScript.CST.SourceRange
      { Language.PureScript.CST.srcStart =
        Language.PureScript.CST.srcStart (sourceRangeFromSourceToken kind')
      , Language.PureScript.CST.srcEnd =
        Language.PureScript.CST.srcEnd (sourceRangeFromName name')
      }
  Language.PureScript.CST.ImportOp _ name' -> sourceRangeFromName name'
  Language.PureScript.CST.ImportType _ name' dataMembers' ->
    Language.PureScript.CST.SourceRange
      { Language.PureScript.CST.srcStart =
        Language.PureScript.CST.srcStart (sourceRangeFromName name')
      , Language.PureScript.CST.srcEnd =
        Language.PureScript.CST.srcEnd
          ( maybe
            (sourceRangeFromName name')
            sourceRangeFromDataMembers
            dataMembers'
          )
      }
  Language.PureScript.CST.ImportTypeOp _ type'' name' ->
    Language.PureScript.CST.SourceRange
      { Language.PureScript.CST.srcStart =
        Language.PureScript.CST.srcStart (sourceRangeFromSourceToken type'')
      , Language.PureScript.CST.srcEnd =
        Language.PureScript.CST.srcEnd (sourceRangeFromName name')
      }
  Language.PureScript.CST.ImportValue _ name' -> sourceRangeFromName name'

sourceRangeFromName ::
  Language.PureScript.CST.Name a ->
  Language.PureScript.CST.SourceRange
sourceRangeFromName =
  sourceRangeFromSourceToken
    . Language.PureScript.CST.nameTok

sourceRangeFromSeparated ::
  (a -> Language.PureScript.CST.SourceRange) ->
  Language.PureScript.CST.Separated a ->
  Language.PureScript.CST.SourceRange
sourceRangeFromSeparated f separated' = case separated' of
  Language.PureScript.CST.Separated head tail ->
    Language.PureScript.CST.SourceRange
      { Language.PureScript.CST.srcStart =
        Language.PureScript.CST.srcStart (f head)
      , Language.PureScript.CST.srcEnd =
        Language.PureScript.CST.srcEnd
          (f $ maybe head snd $ RIO.List.lastMaybe tail)
      }

sourceRangeFromSourceToken ::
  Language.PureScript.CST.SourceToken ->
  Language.PureScript.CST.SourceRange
sourceRangeFromSourceToken =
  Language.PureScript.CST.tokRange
    . Language.PureScript.CST.tokAnn

sourceRangeFromWrapped ::
  Language.PureScript.CST.Wrapped a ->
  Language.PureScript.CST.SourceRange
sourceRangeFromWrapped wrapped' = case wrapped' of
  Language.PureScript.CST.Wrapped open _ close ->
    Language.PureScript.CST.SourceRange
      { Language.PureScript.CST.srcStart =
        Language.PureScript.CST.srcStart (sourceRangeFromSourceToken open)
      , Language.PureScript.CST.srcEnd =
        Language.PureScript.CST.srcEnd (sourceRangeFromSourceToken close)
      }

spanFromSourceRange :: Language.PureScript.CST.SourceRange -> Span
spanFromSourceRange sourceRange = case sourceRange of
  Language.PureScript.CST.SourceRange start end ->
    case linesBetween start end of
      0 -> SingleLine
      _ -> MultipleLines

wrapped :: Language.PureScript.CST.Wrapped a -> Span
wrapped = spanFromSourceRange . sourceRangeFromWrapped
