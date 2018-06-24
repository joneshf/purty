module Purty.Doc.Static where

import "rio" RIO

import "base" Data.List.NonEmpty                 (NonEmpty)
import "semigroupoids" Data.Semigroup.Foldable   (intercalateMap1)
import "prettyprinter" Data.Text.Prettyprint.Doc
    ( Doc
    , align
    , comma
    , dot
    , indent
    , line
    , parens
    , space
    , (<+>)
    )

import qualified "this" Annotation
import qualified "this" AST
import qualified "this" Export
import qualified "this" Name

fromConstructors :: Export.Constructors Annotation.Sorted -> Doc a
fromConstructors = \case
  Export.ConstructorsAnnotation _ann constructors -> fromConstructors constructors
  Export.ConstructorsNone -> mempty
  Export.ConstructorsSome constructors ->
    line <> indent 4 (parenthesize Name.docFromProper constructors)
  Export.ConstructorsAll -> parens (dot <> dot)

fromExport :: Export.Export Annotation.Sorted -> Doc a
fromExport = \case
  Export.ExportAnnotation _ann export -> fromExport export
  Export.ExportClass name -> "class" <+> Name.docFromClass name
  Export.ExportKind name -> "kind" <+> Name.docFromKind name
  Export.ExportModule name -> "module" <+> Name.docFromModule name
  Export.ExportType ty -> fromType ty
  Export.ExportTypeOperator op -> "type" <+> Export.docFromTypeOperator op
  Export.ExportValue value -> Export.docFromValue value
  Export.ExportValueOperator op -> Export.docFromValueOperator op

fromModule :: AST.Module Annotation.Sorted -> Doc a
fromModule = \case
  AST.Module _ann name (Just exports) ->
    "module" <+> Name.docFromModule name
      <> line
      <> indent 2 (parenthesize fromExport exports <+> "where")
      <> line
  AST.Module _ann name Nothing ->
    "module" <+> Name.docFromModule name <+> "where"
      <> line

fromType :: Export.Type Annotation.Sorted -> Doc b
fromType = \case
  Export.Type name constructors ->
    Name.docFromProper name <> fromConstructors constructors

parenthesize :: (a -> Doc b) -> NonEmpty a -> Doc b
parenthesize f xs =
  align (parens (space <> intercalateMap1 (line <> comma <> space) f xs <> line))
