module Declaration
  ( Declarations(Declarations)
  , dynamic
  , fromPureScript
  , normalize
  , static
  ) where

import "rio" RIO hiding (Data)

import "freer-simple" Control.Monad.Freer        (Eff, Members)
import "freer-simple" Control.Monad.Freer.Error  (Error)
import "base" Data.List.NonEmpty                 (NonEmpty)
import "semigroupoids" Data.Semigroup.Foldable   (intercalateMap1)
import "prettyprinter" Data.Text.Prettyprint.Doc (Doc, flatAlt, line)

import qualified "purescript" Language.PureScript

import qualified "this" Annotation
import qualified "this" DataType
import qualified "this" Fixity
import qualified "this" Foreign
import qualified "this" Kind
import qualified "this" Name
import qualified "this" Type
import qualified "this" Variations

data Declaration a
  = DeclarationData !(DataType.Data a)
  | DeclarationFixityType !(Fixity.Type a)
  | DeclarationFixityValue !(Fixity.Value a)
  | DeclarationForeignData !(Foreign.Data a)
  | DeclarationForeignKind !(Foreign.Kind a)
  | DeclarationForeignValue !(Foreign.Value a)
  | DeclarationNewtype !(DataType.Newtype a)
  deriving (Functor)

instance (Display a) => Display (Declaration a) where
  display = \case
    DeclarationData x -> "Declaration Data: " <> display x
    DeclarationFixityType x -> "Declaration Fixity Type: " <> display x
    DeclarationFixityValue x -> "Declaration Fixity Value: " <> display x
    DeclarationForeignData x -> "Declaration Foreign Data: " <> display x
    DeclarationForeignKind x -> "Declaration Foreign Kind: " <> display x
    DeclarationForeignValue x -> "Declaration Foreign Value: " <> display x
    DeclarationNewtype x -> "Declaration Newtype: " <> display x

normalizeDeclaration :: Declaration a -> Declaration Annotation.Normalized
normalizeDeclaration = \case
  DeclarationData x -> DeclarationData (DataType.normalizeData x)
  DeclarationFixityType x -> DeclarationFixityType (Fixity.normalizeType x)
  DeclarationFixityValue x -> DeclarationFixityValue (Fixity.normalizeValue x)
  DeclarationForeignData x -> DeclarationForeignData (Foreign.normalizeData x)
  DeclarationForeignKind x -> DeclarationForeignKind (Foreign.normalizeKind x)
  DeclarationForeignValue x -> DeclarationForeignValue (Foreign.normalizeValue x)
  DeclarationNewtype x -> DeclarationNewtype (DataType.normalizeNewtype x)

fromPureScript ::
  ( Members
    '[ Error DataType.WrongNewtypeConstructors
     , Error Fixity.NegativePrecedence
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
  Language.PureScript.DataDeclaration _ Language.PureScript.Data name variables constructors ->
    Just . DeclarationData <$> DataType.data' name variables constructors
  Language.PureScript.DataDeclaration _ Language.PureScript.Newtype name variables constructors ->
    Just . DeclarationNewtype <$> DataType.newtype' name variables constructors
  Language.PureScript.ExternDataDeclaration _ type' kind ->
    Just . DeclarationForeignData <$> Foreign.data' type' kind
  Language.PureScript.ExternDeclaration _ name type' ->
    Just . DeclarationForeignValue <$> Foreign.value name type'
  Language.PureScript.ExternKindDeclaration _ name ->
    pure (Just $ DeclarationForeignKind $ Foreign.kind name)
  Language.PureScript.FixityDeclaration _ (Left fixity) ->
    Just . DeclarationFixityValue <$> Fixity.value fixity
  Language.PureScript.FixityDeclaration _ (Right fixity) ->
    Just . DeclarationFixityType <$> Fixity.type' fixity
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
  dynamic' = \case
    Declarations Nothing -> mempty
    Declarations (Just declarations) ->
      line
        <> intercalateMap1 line dynamicDoc declarations
  dynamicDoc x =
    flatAlt (Variations.multiLine $ go x) (Variations.singleLine $ go x)
  go = \case
    DeclarationData data' -> pure (DataType.docFromData data')
    DeclarationFixityType type' -> pure (Fixity.docFromType type')
    DeclarationFixityValue value -> pure (Fixity.docFromValue value)
    DeclarationForeignData data' -> pure (Foreign.docFromData data')
    DeclarationForeignKind kind -> pure (Foreign.docFromKind kind)
    DeclarationForeignValue value -> pure (Foreign.docFromValue value)
    DeclarationNewtype newtype' -> pure (DataType.docFromNewtype newtype')
  static' = \case
    Declarations Nothing -> mempty
    Declarations (Just declarations) ->
      line
        <> intercalateMap1 line (Variations.multiLine . go) declarations
