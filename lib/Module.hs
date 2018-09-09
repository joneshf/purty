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

import "freer-simple" Control.Monad.Freer        (Eff, Member, Members)
import "freer-simple" Control.Monad.Freer.Error  (Error, throwError)
import "prettyprinter" Data.Text.Prettyprint.Doc (Doc, line, (<+>))
import "path" Path                               (Abs, File, Path, fromAbsFile)
import "parsec" Text.Parsec                      (ParseError)

import qualified "purescript" Language.PureScript

import qualified "this" Annotation
import qualified "this" Comment
import qualified "this" Declaration
import qualified "this" Declaration.Class
import qualified "this" Declaration.DataType
import qualified "this" Declaration.Fixity
import qualified "this" Declaration.Instance
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
      !Comment.Comments
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
  Module _ann comments name exports imports declarations ->
    Comment.docFromComments comments
      <> "module" <+> Name.docFromModule name <> Export.dynamic exports <+> "where"
      <> line
      <> Import.dynamic imports
      <> Declaration.dynamic declarations

fromPureScript ::
  ( Members Declaration.Class.Errors e
  , Members Declaration.DataType.Errors e
  , Members Declaration.Fixity.Errors e
  , Members Declaration.Instance.Errors e
  , Members Declaration.Value.Errors e
  , Members Export.Errors e
  , Members Kind.Errors e
  , Members Name.Errors e
  , Members Type.Errors e
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
  Language.PureScript.Module _ comments' name' decls exports' -> do
    let comments = Comment.comments comments'
    name <- Name.module' name'
    exports <- Export.exports exports'
    imports <- Import.imports decls
    declarations <- Declaration.declarations decls
    pure (Module Annotation.Unannotated comments name exports imports declarations)

normalize ::
  Module a b (Declaration.Declarations c) d ->
  Module a b (Declaration.Declarations Annotation.Normalized) d
normalize = \case
  Module ann comments name exports imports declarations ->
    Module ann comments name exports imports (Declaration.normalize declarations)

parse ::
  (Member (Error ParseError) e, Member File.File e, Member Log.Log e) =>
  Path Abs File ->
  Eff e Language.PureScript.Module
parse absFile = do
  contents <- File.read absFile
  Log.debug "Read file contents:"
  Log.debug (display contents)
  (_, m) <- parse' absFile contents
  pure m

parse' ::
  (Member (Error ParseError) e) =>
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
  Module ann comments name exports imports declarations ->
    Module ann comments name (Export.sort exports) imports declarations

sortImports :: Module a (Import.Imports b) c d -> Module a Import.Sorted c d
sortImports = \case
  Module ann comments name exports imports declarations ->
    Module ann comments name exports (Import.sort imports) declarations

static ::
  Module
    Export.Sorted
    Import.Sorted
    (Declaration.Declarations Annotation.Normalized) a ->
  Doc b
static = \case
  Module _ann comments name exports imports declarations ->
    Comment.docFromComments comments
      <> "module" <+> Name.docFromModule name <> Export.static exports <+> "where"
      <> line
      <> Import.static imports
      <> Declaration.static declarations
