module Purty.Doc.Dynamic where

import "rio" RIO

import "semigroupoids" Data.Semigroup.Foldable   (intercalateMap1)
import "prettyprinter" Data.Text.Prettyprint.Doc (Doc, line, pretty, (<+>))

import qualified "this" Purty.AST

fromModule :: Purty.AST.Module -> Doc a
fromModule = \case
  Purty.AST.Module name ->
    "module" <+> fromModuleName name <+> "where"
      <> line

fromModuleName :: Purty.AST.ModuleName -> Doc a
fromModuleName = \case
  Purty.AST.ModuleName names -> intercalateMap1 "." fromProperName names

fromProperName :: Purty.AST.ProperName -> Doc a
fromProperName = \case
  Purty.AST.ProperName name -> pretty name
