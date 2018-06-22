module Purty.Doc.Dynamic where

import "rio" RIO

import "semigroupoids" Data.Semigroup.Foldable   (intercalateMap1)
import "prettyprinter" Data.Text.Prettyprint.Doc (Doc, line, pretty, (<+>))

import qualified "this" AST

fromModule :: AST.Module -> Doc a
fromModule = \case
  AST.Module name ->
    "module" <+> fromModuleName name <+> "where"
      <> line

fromModuleName :: AST.ModuleName -> Doc a
fromModuleName = \case
  AST.ModuleName names -> intercalateMap1 "." fromProperName names

fromProperName :: AST.ProperName -> Doc a
fromProperName = \case
  AST.ProperName name -> pretty name
