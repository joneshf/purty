module Purty.Doc.Static where

import "rio" RIO

import "base" Data.List.NonEmpty                 (NonEmpty)
import "semigroupoids" Data.Semigroup.Foldable   (intercalateMap1)
import "prettyprinter" Data.Text.Prettyprint.Doc
    ( Doc
    , align
    , annotate
    , comma
    , indent
    , line
    , lparen
    , pretty
    , rparen
    , space
    , (<+>)
    )

import qualified "this" AST

fromExport :: AST.Export a -> Doc a
fromExport = \case
  AST.ExportAnnotation ann export -> annotate ann (fromExport export)
  AST.ExportModule name -> "module" <+> fromModuleName name
  AST.ExportValue ident -> fromIdent ident

fromIdent :: AST.Ident -> Doc a
fromIdent = \case
  AST.Ident name -> pretty name

fromModule :: AST.Module a -> Doc a
fromModule = \case
  AST.Module ann name (Just exports) -> annotate ann doc
    where
    doc =
      "module" <+> fromModuleName name
        <> line
        <> indent 2 (parenthesize fromExport exports <+> "where")
        <> line
  AST.Module ann name Nothing -> annotate ann doc
    where
    doc =
      "module" <+> fromModuleName name <+> "where"
        <> line

fromModuleName :: AST.ModuleName a -> Doc a
fromModuleName = \case
  AST.ModuleName names -> intercalateMap1 "." fromProperName names

fromProperName :: AST.ProperName a -> Doc a
fromProperName = \case
  AST.ProperName ann name -> annotate ann (pretty name)

parenthesize :: (a -> Doc b) -> NonEmpty a -> Doc b
parenthesize f xs = align (lparen <+> ys <> line <> rparen)
  where
  ys = intercalateMap1 (line <> comma <> space) f xs
