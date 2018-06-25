module Module where

import "rio" RIO

import "lens" Control.Lens                       (Prism', prism)
import "mtl" Control.Monad.Except                (MonadError)
import "base" Data.List.NonEmpty                 (NonEmpty, nonEmpty)
import "semigroupoids" Data.Semigroup.Foldable   (intercalateMap1)
import "prettyprinter" Data.Text.Prettyprint.Doc (Doc, line, (<+>))
import "witherable" Data.Witherable              (wither)

import qualified "purescript" Language.PureScript

import qualified "this" Annotation
import qualified "this" Export
import qualified "this" Import
import qualified "this" Name

data Module imports a
  = Module
      !a
      !(Name.Module a)
      !(Maybe (NonEmpty (Export.Export a)))
      !imports
  deriving (Functor)

instance (Display a, Display b) => Display (Module a b) where
  display = \case
    Module ann name exports imports ->
      "{Module "
        <> "annotation: "
        <> display ann
        <> ", name: "
        <> display name
        <> foldMap (\x -> ", exports: " <> intercalateMap1 ", " display x) exports
        <> ", imports: "
        <> display imports
        <> "}"

dynamic :: Module Import.Sorted Annotation.Sorted -> Doc a
dynamic = \case
  Module _ann name exports imports ->
    "module" <+> Name.docFromModule name <> Export.dynamic exports <+> "where"
      <> Import.dynamic imports
      <> line

fromPureScript ::
  (Export.IsError e, Name.IsMissing e, MonadError e f) =>
  Language.PureScript.Module ->
  f (Module (Import.Imports Annotation.Unannotated) Annotation.Unannotated)
fromPureScript = \case
  Language.PureScript.Module _ _ name' decls exports' -> do
    name <- Name.module' name'
    exports <- traverse Export.fromPureScript exports'
    imports <- Import.Imports . nonEmpty <$> wither Import.fromPureScript decls
    pure (Module Annotation.Unannotated name exports imports)

sortExports ::
  Module (Import.Imports a) b ->
  Module Import.Sorted Annotation.Sorted
sortExports = \case
  Module _ann name exports imports ->
    Module
      Annotation.Sorted
      (Annotation.Sorted <$ name)
      (fmap Export.sort exports)
      (Import.sort imports)

static :: Module Import.Sorted Annotation.Sorted -> Doc a
static = \case
  Module _ann name exports imports ->
    "module" <+> Name.docFromModule name <> Export.static exports <+> "where"
      <> Import.static imports
      <> line

-- Errors

data Error
  = NotImplemented !Utf8Builder

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
