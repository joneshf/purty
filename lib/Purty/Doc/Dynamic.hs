module Purty.Doc.Dynamic where

import "rio" RIO

import "base" Data.List.NonEmpty                 (NonEmpty)
import "semigroupoids" Data.Semigroup.Foldable   (intercalateMap1)
import "prettyprinter" Data.Text.Prettyprint.Doc
    ( Doc
    , align
    , annotate
    , comma
    , flatAlt
    , group
    , indent
    , line
    , lparen
    , pretty
    , rparen
    , space
    , (<+>)
    )

import qualified "this" AST

data Variations a
  = Variations
    { multiLine  :: a
    , singleLine :: a
    }

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
  AST.Module ann name (Just exports') -> annotate ann doc
    where
    doc =
      "module" <+> fromModuleName name <> group (flatAlt multi single)
        <> line
    multi = line <> indent 2 (multiLine <+> "where")
    single = space <> singleLine <+> "where"
    Variations { multiLine, singleLine } = parenthesize fromExport exports'
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

parenthesize :: (a -> Doc b) -> NonEmpty a -> Variations (Doc b)
parenthesize f xs = Variations { multiLine, singleLine }
  where
  multiLine =
    align
      ( lparen
      <+> intercalateMap1 (line <> comma <> space) f xs
      <> line
      <> rparen
      )
  singleLine = lparen <> intercalateMap1 (comma <> space) f xs <> rparen
