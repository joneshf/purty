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
    , pretty
    , space
    , (<+>)
    )

import qualified "this" Annotation
import qualified "this" AST

data Variations a
  = Variations
    { multiLine  :: !a
    , singleLine :: !a
    }

fromClassName :: AST.ClassName a -> Doc b
fromClassName = \case
  AST.ClassName name -> fromProperName name

fromConstructors :: AST.Constructors Annotation.Sorted -> Doc a
fromConstructors = \case
  AST.ConstructorsAnnotation _ann constructors -> fromConstructors constructors
  AST.ConstructorsNone -> mempty
  AST.ConstructorsSome constructors -> group (flatAlt multiLine singleLine)
    where
    Variations { multiLine, singleLine } =
      parenthesize fromProperName constructors
  AST.ConstructorsAll -> parens (dot <> dot)

fromExport :: AST.Export Annotation.Sorted -> Doc a
fromExport = \case
  AST.ExportAnnotation _ann export -> fromExport export
  AST.ExportClass name -> "class" <+> fromClassName name
  AST.ExportKind name -> "kind" <+> fromKindName name
  AST.ExportModule name -> "module" <+> fromModuleName name
  AST.ExportType ty -> fromType ty
  AST.ExportTypeOperator op -> "type" <+> fromTypeOperator op
  AST.ExportValue ident -> fromIdent ident
  AST.ExportValueOperator op -> fromValueOperator op

fromIdent :: AST.Ident -> Doc a
fromIdent = \case
  AST.Ident name -> pretty name

fromKindName :: AST.KindName a -> Doc b
fromKindName = \case
  AST.KindName name -> fromProperName name

fromModule :: AST.Module Annotation.Sorted -> Doc a
fromModule = \case
  AST.Module _ann name (Just exports') -> doc
    where
    doc =
      "module" <+> fromModuleName name <> group (flatAlt multi single)
        <> line
    multi = line <> indent 2 (multiLine <+> "where")
    single = space <> singleLine <+> "where"
    Variations { multiLine, singleLine } = parenthesize fromExport exports'
  AST.Module _ann name Nothing ->
    "module" <+> fromModuleName name <+> "where"
      <> line

fromModuleName :: AST.ModuleName a -> Doc b
fromModuleName = \case
  AST.ModuleName names -> intercalateMap1 "." fromProperName names

fromProperName :: AST.ProperName a -> Doc b
fromProperName = \case
  AST.ProperName _ann name -> pretty name

fromType :: AST.Type Annotation.Sorted -> Doc b
fromType = \case
  AST.Type name constructors ->
    fromProperName name <> fromConstructors constructors

fromTypeOperator :: AST.TypeOperator a -> Doc b
fromTypeOperator = \case
  AST.TypeOperator _ann op -> parens (pretty op)

fromValueOperator :: AST.ValueOperator a -> Doc b
fromValueOperator = \case
  AST.ValueOperator _ann op -> parens (pretty op)

parenthesize :: (a -> Doc b) -> NonEmpty a -> Variations (Doc b)
parenthesize f xs = Variations { multiLine, singleLine }
  where
  multiLine =
    align
      (parens (space <> intercalateMap1 (line <> comma <> space) f xs <> line))
  singleLine = parens (intercalateMap1 (comma <> space) f xs)
