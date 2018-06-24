module Purty.Doc.Dynamic where

import "rio" RIO

import "base" Data.List.NonEmpty                 (NonEmpty)
import "semigroupoids" Data.Semigroup.Foldable   (intercalateMap1)
import "prettyprinter" Data.Text.Prettyprint.Doc
    ( Doc
    , align
    , comma
    , dot
    , flatAlt
    , group
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

data Variations a
  = Variations
    { multiLine  :: !a
    , singleLine :: !a
    }

fromConstructors :: Export.Constructors Annotation.Sorted -> Doc a
fromConstructors = \case
  Export.ConstructorsAnnotation _ann constructors -> fromConstructors constructors
  Export.ConstructorsNone -> mempty
  Export.ConstructorsSome constructors -> group (flatAlt multiLine singleLine)
    where
    Variations { multiLine, singleLine } =
      parenthesize Name.docFromProper constructors
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
  AST.Module _ann name (Just exports') -> doc
    where
    doc =
      "module" <+> Name.docFromModule name <> group (flatAlt multi single)
        <> line
    multi = line <> indent 2 (multiLine <+> "where")
    single = space <> singleLine <+> "where"
    Variations { multiLine, singleLine } = parenthesize fromExport exports'
  AST.Module _ann name Nothing ->
    "module" <+> Name.docFromModule name <+> "where"
      <> line

fromType :: Export.Type Annotation.Sorted -> Doc b
fromType = \case
  Export.Type name constructors ->
    Name.docFromProper name <> fromConstructors constructors

parenthesize :: (a -> Doc b) -> NonEmpty a -> Variations (Doc b)
parenthesize f xs = Variations { multiLine, singleLine }
  where
  multiLine =
    align
      (parens (space <> intercalateMap1 (line <> comma <> space) f xs <> line))
  singleLine = parens (intercalateMap1 (comma <> space) f xs)
