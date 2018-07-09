module Module
  ( dynamic
  , fromPureScript
  , normalize
  , parse
  , sortExports
  , sortImports
  , static
  ) where

import "rio" RIO

import "freer-simple" Control.Monad.Freer        (Eff, Members)
import "freer-simple" Control.Monad.Freer.Error  (Error, throwError)
import "base" Data.List.NonEmpty                 (nonEmpty)
import "freer-simple" Data.OpenUnion             ((:++:))
import "prettyprinter" Data.Text.Prettyprint.Doc (Doc, line, (<+>))
import "witherable" Data.Witherable              (wither)
import "path" Path                               (Abs, File, Path, fromAbsFile)
import "parsec" Text.Parsec                      (ParseError)

import qualified "purescript" Language.PureScript

import qualified "this" Annotation
import qualified "this" Declaration
import qualified "this" Declaration.Class
import qualified "this" Declaration.DataType
import qualified "this" Declaration.Fixity
import qualified "this" Declaration.Value
import qualified "this" Export
import qualified "this" File
import qualified "this" Import
import qualified "this" Kind
import qualified "this" Log
import qualified "this" Name
import qualified "this" Type

data Module exports imports declarations a
  = Module
      !a
      !(Name.Module a)
      !exports
      !imports
      !declarations
  deriving (Functor, Show)

instance
  ( Log.Inspect a
  , Log.Inspect b
  , Log.Inspect c
  , Log.Inspect d
  ) =>
  Log.Inspect (Module a b c d)

dynamic ::
  Module
    Export.Sorted
    Import.Sorted
    (Declaration.Declarations Annotation.Normalized)
    a ->
  Doc b
dynamic = \case
  Module _ann name exports imports declarations ->
    "module" <+> Name.docFromModule name <> Export.dynamic exports <+> "where"
      <> line
      <> Import.dynamic imports
      <> Declaration.dynamic declarations

fromPureScript ::
  ( Members
    ( Declaration.Class.Errors
    :++: Declaration.DataType.Errors
    :++: Declaration.Fixity.Errors
    :++: Declaration.Value.Errors
    :++: Export.Errors
    :++: Kind.Errors
    :++: Name.Errors
    :++: Type.Errors
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
  Module a b (Declaration.Declarations Annotation.Normalized) d
normalize = \case
  Module ann name exports imports declarations ->
    Module ann name exports imports (Declaration.normalize declarations)

parse ::
  (Members '[Error ParseError, File.File, Log.Log] e) =>
  Path Abs File ->
  Eff e Language.PureScript.Module
parse absFile = do
  contents <- File.read absFile
  Log.debug "Read file contents:"
  Log.debug (display contents)
  (_, m) <- parse' absFile contents
  pure m

parse' ::
  (Members '[Error ParseError] e) =>
  Path Abs File ->
  Text ->
  Eff e (FilePath, Language.PureScript.Module)
parse' absFile contents =
  either
    throwError
    pure
    (Language.PureScript.parseModuleFromFile id (fromAbsFile absFile, contents))

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
    (Declaration.Declarations Annotation.Normalized) a ->
  Doc b
static = \case
  Module _ann name exports imports declarations ->
    "module" <+> Name.docFromModule name <> Export.static exports <+> "where"
      <> line
      <> Import.static imports
      <> Declaration.static declarations

-- Errors

newtype NotImplemented
  = NotImplemented Utf8Builder

instance Display NotImplemented where
  display = \case
    NotImplemented x -> "We have not yet implemented: " <> x
