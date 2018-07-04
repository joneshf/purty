module Module where

import "rio" RIO

import "freer-simple" Control.Monad.Freer        (Eff, Members)
import "base" Data.List.NonEmpty                 (nonEmpty)
import "freer-simple" Data.OpenUnion             ((:++:))
import "prettyprinter" Data.Text.Prettyprint.Doc (Doc, line, (<+>))
import "witherable" Data.Witherable              (wither)

import qualified "purescript" Language.PureScript

import qualified "this" Annotation
import qualified "this" Declaration
import qualified "this" Export
import qualified "this" Import
import qualified "this" Name

data Module exports imports declarations a
  = Module
      !a
      !(Name.Module a)
      !exports
      !imports
      !declarations
  deriving (Functor)

instance
  ( Display a
  , Display b
  , Display c
  , Display d
  ) =>
  Display (Module a b c d) where
    display = \case
      Module ann name exports imports declarations ->
        "{Module "
          <> "annotation: "
          <> display ann
          <> ", name: "
          <> display name
          <> ", exports: "
          <> display exports
          <> ", imports: "
          <> display imports
          <> ", declarations: "
          <> display declarations
          <> "}"

dynamic ::
  Module
    Export.Sorted
    Import.Sorted
    (Declaration.Declarations Declaration.Normalized)
    a ->
  Doc b
dynamic = \case
  Module _ann name exports imports declarations ->
    "module" <+> Name.docFromModule name <> Export.dynamic exports <+> "where"
      <> Import.dynamic imports
      <> Declaration.dynamic declarations
      <> line

fromPureScript ::
  ( Members
    ( Declaration.Errors
    :++: Export.Errors
    :++: Name.Errors
    )
    e
  ) =>
  Language.PureScript.Module ->
  Eff
    e
    (Module
      (Export.Exports Annotation.Unannotated)
      (Import.Imports Annotation.Unannotated)
      (Declaration.Declarations Annotation.Unannotated)
      Annotation.Unannotated
    )
fromPureScript = \case
  Language.PureScript.Module _ _ name' decls exports' -> do
    name <- Name.module' name'
    exports <- Export.Exports <$> traverse Export.fromPureScript exports'
    imports <- Import.Imports . nonEmpty <$> wither Import.fromPureScript decls
    declarations <-
      Declaration.Declarations . nonEmpty <$> wither Declaration.fromPureScript decls
    pure (Module Annotation.Unannotated name exports imports declarations)

normalize ::
  Module a b (Declaration.Declarations c) d ->
  Module a b (Declaration.Declarations Declaration.Normalized) d
normalize = \case
  Module ann name exports imports declarations ->
    Module ann name exports imports (Declaration.normalize declarations)

sortExports :: Module (Export.Exports a) b c d -> Module Export.Sorted b c d
sortExports = \case
  Module ann name exports imports declarations ->
    Module ann name (Export.sort exports) imports declarations

sortImports :: Module a (Import.Imports b) c d -> Module a Import.Sorted c d
sortImports = \case
  Module ann name exports imports declarations ->
    Module ann name exports (Import.sort imports) declarations

static ::
  Module
    Export.Sorted
    Import.Sorted
    (Declaration.Declarations Declaration.Normalized) a ->
  Doc b
static = \case
  Module _ann name exports imports declarations ->
    "module" <+> Name.docFromModule name <> Export.static exports <+> "where"
      <> Import.static imports
      <> Declaration.static declarations
      <> line

-- Errors

newtype NotImplemented
  = NotImplemented Utf8Builder

instance Display NotImplemented where
  display = \case
    NotImplemented x -> "We have not yet implemented: " <> x
