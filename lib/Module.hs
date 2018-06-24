module Module where

import "rio" RIO

import "lens" Control.Lens                       (Prism', prism)
import "mtl" Control.Monad.Except                (MonadError)
import "base" Data.List.NonEmpty                 (NonEmpty)
import "semigroupoids" Data.Semigroup.Foldable   (intercalateMap1)
import "prettyprinter" Data.Text.Prettyprint.Doc (Doc, line, (<+>))

import qualified "purescript" Language.PureScript

import qualified "this" Annotation
import qualified "this" Export
import qualified "this" Name

data Module a
  = Module !a !(Name.Module a) !(Maybe (NonEmpty (Export.Export a)))
  deriving (Functor)

instance (Display a) => Display (Module a) where
  display = \case
    Module ann name exports ->
      "{Module "
        <> "annotation: "
        <> display ann
        <> ", name: "
        <> display name
        <> foldMap (\x -> ", exports: " <> intercalateMap1 ", " display x) exports
        <> "}"

dynamic :: Module Annotation.Sorted -> Doc a
dynamic = \case
  Module _ann name exports ->
    "module" <+> Name.docFromModule name <> Export.dynamic exports <+> "where"
      <> line

fromPureScript ::
  (Export.IsError e, Name.IsMissing e, MonadError e f) =>
  Language.PureScript.Module ->
  f (Module Annotation.Unannotated)
fromPureScript = \case
  Language.PureScript.Module _ _ name' _ exports' -> do
    name <- Name.module' name'
    exports <- traverse Export.fromPureScript exports'
    pure (Module Annotation.Unannotated name exports)

sortExports :: Module a -> Module Annotation.Sorted
sortExports = \case
  Module _ann name exports ->
    Module Annotation.Sorted (Annotation.Sorted <$ name) (fmap Export.sort exports)

static :: Module Annotation.Sorted -> Doc a
static = \case
  Module _ann name exports ->
    "module" <+> Name.docFromModule name <> Export.static exports <+> "where"
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
