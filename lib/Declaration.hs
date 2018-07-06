module Declaration
  ( Declarations(Declarations)
  , dynamic
  , fromPureScript
  , normalize
  , static
  ) where

import "rio" RIO hiding (Data)

import "freer-simple" Control.Monad.Freer        (Eff, Members)
import "freer-simple" Control.Monad.Freer.Error  (Error, throwError)
import "base" Data.Bitraversable                 (bitraverse)
import "base" Data.List.NonEmpty                 (NonEmpty, nonEmpty)
import "semigroupoids" Data.Semigroup.Foldable   (intercalateMap1)
import "prettyprinter" Data.Text.Prettyprint.Doc (Doc, line)

import qualified "purescript" Language.PureScript

import qualified "this" Annotation
import qualified "this" DataType
import qualified "this" Foreign
import qualified "this" Kind
import qualified "this" Name
import qualified "this" Type

data Declaration a
  = DeclarationData !(DataType.Data a)
  | DeclarationForeignData !(Foreign.Data a)
  | DeclarationForeignKind !(Foreign.Kind a)
  | DeclarationForeignValue !(Foreign.Value a)
  | DeclarationNewtype !(DataType.Newtype a)
  deriving (Functor)

instance (Display a) => Display (Declaration a) where
  display = \case
    DeclarationData x -> "Declaration Data: " <> display x
    DeclarationForeignData x -> "Declaration Foreign Data: " <> display x
    DeclarationForeignKind x -> "Declaration Foreign Kind: " <> display x
    DeclarationForeignValue x -> "Declaration Foreign Value: " <> display x
    DeclarationNewtype x -> "Declaration Newtype: " <> display x

normalizeDeclaration :: Declaration a -> Declaration Annotation.Normalized
normalizeDeclaration = \case
  DeclarationData x -> DeclarationData (DataType.normalizeData x)
  DeclarationForeignData x -> DeclarationForeignData (Foreign.normalizeData x)
  DeclarationForeignKind x -> DeclarationForeignKind (Foreign.normalizeKind x)
  DeclarationForeignValue x -> DeclarationForeignValue (Foreign.normalizeValue x)
  DeclarationNewtype x -> DeclarationNewtype (DataType.normalizeNewtype x)

fromPureScript ::
  ( Members
    '[ Error DataType.WrongNewtypeConstructors
     , Error Kind.InferredKind
     , Error Name.InvalidCommon
     , Error Name.Missing
     , Error Type.InferredConstraintData
     , Error Type.InferredForallWithSkolem
     , Error Type.InferredSkolem
     , Error Type.InferredType
     , Error Type.InfixTypeNotTypeOp
     , Error Type.PrettyPrintForAll
     , Error Type.PrettyPrintFunction
     , Error Type.PrettyPrintObject
     ]
    e
  ) =>
  Language.PureScript.Declaration ->
  Eff e (Maybe (Declaration Annotation.Unannotated))
fromPureScript = \case
  Language.PureScript.BoundValueDeclaration {} -> pure Nothing
  Language.PureScript.DataDeclaration _ Language.PureScript.Data name' variables' constructors -> do
    alternates <- nonEmpty <$> traverse DataType.alternate constructors
    variables <- traverse (bitraverse (pure . Type.Variable) (traverse Kind.fromPureScript)) variables'
    let data' = DataType.Data name (Type.Variables $ nonEmpty variables) alternates
        name = Name.proper name'
    pure (Just $ DeclarationData data')
  Language.PureScript.DataDeclaration _ Language.PureScript.Newtype name' variables' [(constructor', [ty'])] -> do
    ty <- Type.fromPureScript ty'
    variables <- traverse (bitraverse (pure . Type.Variable) (traverse Kind.fromPureScript)) variables'
    let constructor = Name.constructor constructor'
        name = Name.proper name'
        newtype' =
          DataType.Newtype name (Type.Variables $ nonEmpty variables) constructor ty
    pure (Just $ DeclarationNewtype newtype')
  Language.PureScript.DataDeclaration _ Language.PureScript.Newtype name _ constructors ->
    throwError (DataType.WrongNewtypeConstructors name constructors)
  Language.PureScript.ExternDataDeclaration _ type'' kind' -> do
    kind <- Kind.fromPureScript kind'
    let data' = Foreign.Data type' kind
        type' = Name.type' type''
    pure (Just $ DeclarationForeignData data')
  Language.PureScript.ExternDeclaration _ name' type'' -> do
    name <- Name.common name'
    type' <- Type.fromPureScript type''
    let value = Foreign.Value name type'
    pure (Just $ DeclarationForeignValue value)
  Language.PureScript.ExternKindDeclaration _ name' -> do
    let kind = Foreign.Kind name
        name = Name.kind name'
    pure (Just $ DeclarationForeignKind kind)
  Language.PureScript.FixityDeclaration {} -> pure Nothing
  Language.PureScript.TypeClassDeclaration {} -> pure Nothing
  Language.PureScript.TypeDeclaration {} -> pure Nothing
  Language.PureScript.TypeInstanceDeclaration {} -> pure Nothing
  Language.PureScript.TypeSynonymDeclaration {} -> pure Nothing
  Language.PureScript.ValueDeclaration {} -> pure Nothing
  Language.PureScript.BindingGroupDeclaration {} -> pure Nothing
  Language.PureScript.DataBindingGroupDeclaration {} -> pure Nothing
  Language.PureScript.ImportDeclaration {} -> pure Nothing

newtype Declarations a
  = Declarations (Maybe (NonEmpty (Declaration a)))

instance (Display a) => Display (Declarations a) where
  display = \case
    Declarations Nothing -> "No Declarations"
    Declarations (Just declarations) ->
      "Declarations ["
        <> intercalateMap1 ", " display declarations
        <> "]"

normalize :: Declarations a -> Declarations Annotation.Normalized
normalize = \case
  Declarations declarations' ->
    Declarations ((fmap . fmap) normalizeDeclaration declarations')

dynamic, static :: Declarations Annotation.Normalized -> Doc a
(dynamic, static) = (dynamic', static')
  where
  dynamic' = static'
  go = \case
    DeclarationData data' -> DataType.docFromData data'
    DeclarationForeignData data' -> Foreign.docFromData data'
    DeclarationForeignKind kind -> Foreign.docFromKind kind
    DeclarationForeignValue value -> Foreign.docFromValue value
    DeclarationNewtype newtype' -> DataType.docFromNewtype newtype'
  static' = \case
    Declarations Nothing -> mempty
    Declarations (Just declarations) ->
      line
        <> line
        <> intercalateMap1 line go declarations
