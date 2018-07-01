module Module where

import "rio" RIO

import "lens" Control.Lens                       (Prism', prism)
import "mtl" Control.Monad.Except                (MonadError)
import "base" Data.List.NonEmpty                 (nonEmpty)
import "prettyprinter" Data.Text.Prettyprint.Doc (Doc, line, (<+>))
import "witherable" Data.Witherable              (wither)

import qualified "purescript" Language.PureScript

import qualified "this" Annotation
import qualified "this" Export
import qualified "this" Import
import qualified "this" Name

data Module exports imports a
  = Module
      !a
      !(Name.Module a)
      !exports
      !imports
  deriving (Functor)

instance (Display a, Display b, Display c) => Display (Module a b c) where
  display = \case
    Module ann name exports imports ->
      "{Module "
        <> "annotation: "
        <> display ann
        <> ", name: "
        <> display name
        <> ", exports: "
        <> display exports
        <> ", imports: "
        <> display imports
        <> "}"

dynamic :: Module Export.Sorted Import.Sorted a -> Doc b
dynamic = \case
  Module _ann name exports imports ->
    "module" <+> Name.docFromModule name <> Export.dynamic exports <+> "where"
      <> Import.dynamic imports
      <> line

fromPureScript ::
  (Export.IsError e, Name.IsMissing e, MonadError e f) =>
  Language.PureScript.Module ->
  f (Module (Export.Exports Annotation.Unannotated) (Import.Imports Annotation.Unannotated) Annotation.Unannotated)
fromPureScript = \case
  Language.PureScript.Module _ _ name' decls exports' -> do
    name <- Name.module' name'
    exports <- Export.Exports <$> traverse Export.fromPureScript exports'
    imports <- Import.Imports . nonEmpty <$> wither Import.fromPureScript decls
    pure (Module Annotation.Unannotated name exports imports)

sortExports :: Module (Export.Exports a) b c -> Module Export.Sorted b c
sortExports = \case
  Module ann name exports imports ->
    Module ann name (Export.sort exports) imports

sortImports :: Module a (Import.Imports b) c -> Module a Import.Sorted c
sortImports = \case
  Module ann name exports imports ->
    Module ann name exports (Import.sort imports)

static :: Module Export.Sorted Import.Sorted a -> Doc b
static = \case
  Module _ann name exports imports ->
    "module" <+> Name.docFromModule name <> Export.static exports <+> "where"
      <> Import.static imports
      <> line

-- Errors

newtype Error
  = NotImplemented Utf8Builder

instance Display Error where
  display = \case
    NotImplemented x -> "We have not yet implemented: " <> x

class (IsNotImplemented error) => IsError error where
    _Error :: Prism' error Error

instance IsError Error where
  _Error = prism id Right

class IsNotImplemented error where
  _NotImplemented :: Prism' error Utf8Builder

instance IsNotImplemented Error where
  _NotImplemented = prism NotImplemented $ \case
    NotImplemented x -> Right x
