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
    , pretty
    , space
    , (<+>)
    )

import qualified "this" AST

fromConstructors :: AST.Constructors AST.Sorted -> Doc a
fromConstructors = \case
  AST.ConstructorsAnnotation _ann constructors -> fromConstructors constructors
  AST.ConstructorsNone -> mempty
  AST.ConstructorsSome constructors ->
    line <> indent 4 (parenthesize fromProperName constructors)
  AST.ConstructorsAll -> parens (dot <> dot)

fromExport :: AST.Export AST.Sorted -> Doc a
fromExport = \case
  AST.ExportAnnotation _ann export -> fromExport export
  AST.ExportKind name -> "kind" <+> fromKindName name
  AST.ExportModule name -> "module" <+> fromModuleName name
  AST.ExportType ty -> fromType ty
  AST.ExportTypeOperator op -> "type" <+> fromTypeOperator op
  AST.ExportValue ident -> fromIdent ident

fromIdent :: AST.Ident -> Doc a
fromIdent = \case
  AST.Ident name -> pretty name

fromKindName :: AST.KindName a -> Doc b
fromKindName = \case
  AST.KindName name -> fromProperName name

fromModule :: AST.Module AST.Sorted -> Doc a
fromModule = \case
  AST.Module _ann name (Just exports) ->
    "module" <+> fromModuleName name
      <> line
      <> indent 2 (parenthesize fromExport exports <+> "where")
      <> line
  AST.Module _ann name Nothing ->
    "module" <+> fromModuleName name <+> "where"
      <> line

fromModuleName :: AST.ModuleName a -> Doc b
fromModuleName = \case
  AST.ModuleName names -> intercalateMap1 "." fromProperName names

fromProperName :: AST.ProperName a -> Doc b
fromProperName = \case
  AST.ProperName _ann name -> pretty name

fromType :: AST.Type AST.Sorted -> Doc b
fromType = \case
  AST.Type name constructors ->
    fromProperName name <> fromConstructors constructors

fromTypeOperator :: AST.TypeOperator a -> Doc b
fromTypeOperator = \case
  AST.TypeOperator _ann op -> parens (pretty op)

parenthesize :: (a -> Doc b) -> NonEmpty a -> Doc b
parenthesize f xs =
  align (parens (space <> intercalateMap1 (line <> comma <> space) f xs <> line))
