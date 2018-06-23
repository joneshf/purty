module Purty.Doc.Static where

import "rio" RIO

import "base" Data.List.NonEmpty                 (NonEmpty)
import "semigroupoids" Data.Semigroup.Foldable   (intercalateMap1)
import "prettyprinter" Data.Text.Prettyprint.Doc
    (space,  Doc
    , align
    , comma
    , indent
    , line
    , lparen
    , pretty
    , rparen
    , (<+>)
    )

import qualified "this" AST

fromExport :: AST.Export -> Doc a
fromExport = \case
  AST.ExportModule name -> "module" <+> fromModuleName name
  AST.ExportValue ident -> fromIdent ident

fromIdent :: AST.Ident -> Doc a
fromIdent = \case
  AST.Ident name -> pretty name

fromModule :: AST.Module -> Doc a
fromModule = \case
  AST.Module name (Just exports) ->
    "module" <+> fromModuleName name
      <> line
      <> indent 2 (parenthesize fromExport exports <+> "where")
      <> line
  AST.Module name Nothing ->
    "module" <+> fromModuleName name <+> "where"
      <> line

fromModuleName :: AST.ModuleName -> Doc a
fromModuleName = \case
  AST.ModuleName names -> intercalateMap1 "." fromProperName names

fromProperName :: AST.ProperName -> Doc a
fromProperName = \case
  AST.ProperName name -> pretty name

parenthesize :: (a -> Doc b) -> NonEmpty a -> Doc b
parenthesize f xs = align (lparen <+> ys <> line <> rparen)
  where
  ys = intercalateMap1 (line <> comma <> space) f xs
