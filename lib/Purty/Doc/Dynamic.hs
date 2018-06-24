module Purty.Doc.Dynamic where

import "rio" RIO

import "base" Data.List.NonEmpty                 (NonEmpty)
import "semigroupoids" Data.Semigroup.Foldable   (intercalateMap1)
import "prettyprinter" Data.Text.Prettyprint.Doc
    ( Doc
    , align
    , comma
    , flatAlt
    , group
    , indent
    , line
    , parens
    , pretty
    , space
    , (<+>)
    )

import qualified "this" AST

data Variations a
  = Variations
    { multiLine  :: !a
    , singleLine :: !a
    }

fromExport :: AST.Export AST.Sorted -> Doc a
fromExport = \case
  AST.ExportAnnotation _ann export -> fromExport export
  AST.ExportKind name -> "kind" <+> fromKindName name
  AST.ExportModule name -> "module" <+> fromModuleName name
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

fromTypeOperator :: AST.TypeOperator a -> Doc b
fromTypeOperator = \case
  AST.TypeOperator _ann op -> parens (pretty op)

parenthesize :: (a -> Doc b) -> NonEmpty a -> Variations (Doc b)
parenthesize f xs = Variations { multiLine, singleLine }
  where
  multiLine =
    align
      (parens (space <> intercalateMap1 (line <> comma <> space) f xs <> line))
  singleLine = parens (intercalateMap1 (comma <> space) f xs)
