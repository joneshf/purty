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
import "prettyprinter" Data.Text.Prettyprint.Doc (Doc, flatAlt, group, line)

import qualified "purescript" Language.PureScript

import qualified "this" Annotation
import qualified "this" Declaration.DataType
import qualified "this" Declaration.Fixity
import qualified "this" Declaration.Foreign
import qualified "this" Declaration.Synonym
import qualified "this" Kind
import qualified "this" Name
import qualified "this" Type
import qualified "this" Variations

data Declaration a
  = DeclarationData !(Declaration.DataType.Data a)
  | DeclarationFixityType !(Declaration.Fixity.Type a)
  | DeclarationFixityValue !(Declaration.Fixity.Value a)
  | DeclarationForeignData !(Declaration.Foreign.Data a)
  | DeclarationForeignKind !(Declaration.Foreign.Kind a)
  | DeclarationForeignValue !(Declaration.Foreign.Value a)
  | DeclarationNewtype !(Declaration.DataType.Newtype a)
  | DeclarationSynonym !(Declaration.Synonym.Synonym a)
  | DeclarationType !(Type.Declaration a)
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
    DeclarationSynonym x -> "Declaration Synonym: " <> display x
    DeclarationType x -> "Declaration Type: " <> display x

normalizeDeclaration :: Declaration a -> Declaration Annotation.Normalized
normalizeDeclaration = \case
  DeclarationData x -> DeclarationData (Declaration.DataType.normalizeData x)
  DeclarationFixityType x ->
    DeclarationFixityType (Declaration.Fixity.normalizeType x)
  DeclarationFixityValue x ->
    DeclarationFixityValue (Declaration.Fixity.normalizeValue x)
  DeclarationForeignData x ->
    DeclarationForeignData (Declaration.Foreign.normalizeData x)
  DeclarationForeignKind x ->
    DeclarationForeignKind (Declaration.Foreign.normalizeKind x)
  DeclarationForeignValue x ->
    DeclarationForeignValue (Declaration.Foreign.normalizeValue x)
  DeclarationNewtype x ->
    DeclarationNewtype (Declaration.DataType.normalizeNewtype x)
  DeclarationSynonym x -> DeclarationSynonym (Declaration.Synonym.normalize x)
  DeclarationType x -> DeclarationType (Type.normalizeDeclaration x)

fromPureScript ::
  ( Members
    '[ Error Declaration.DataType.WrongNewtypeConstructors
     , Error Declaration.Fixity.NegativePrecedence
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
    Just . DeclarationData <$> Declaration.DataType.data' name variables constructors
  Language.PureScript.DataDeclaration _ Language.PureScript.Newtype name variables constructors ->
    Just . DeclarationNewtype <$> Declaration.DataType.newtype' name variables constructors
  Language.PureScript.ExternDataDeclaration _ type' kind ->
    Just . DeclarationForeignData <$> Declaration.Foreign.data' type' kind
  Language.PureScript.ExternDeclaration _ name type' ->
    Just . DeclarationForeignValue <$> Declaration.Foreign.value name type'
  Language.PureScript.ExternKindDeclaration _ name ->
    pure (Just $ DeclarationForeignKind $ Declaration.Foreign.kind name)
  Language.PureScript.FixityDeclaration _ (Left fixity) ->
    Just . DeclarationFixityValue <$> Declaration.Fixity.value fixity
  Language.PureScript.FixityDeclaration _ (Right fixity) ->
    Just . DeclarationFixityType <$> Declaration.Fixity.type' fixity
  Language.PureScript.TypeClassDeclaration {} -> pure Nothing
  Language.PureScript.TypeDeclaration declaration ->
    Just . DeclarationType <$> Type.declaration declaration
  Language.PureScript.TypeInstanceDeclaration {} -> pure Nothing
  Language.PureScript.TypeSynonymDeclaration _ name variables type' ->
    Just . DeclarationSynonym <$> Declaration.Synonym.fromPureScript name variables type'
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
  dynamicDoc = \case
    DeclarationData data' -> Declaration.DataType.docFromData data'
    DeclarationFixityType type' -> Declaration.Fixity.docFromType type'
    DeclarationFixityValue value -> Declaration.Fixity.docFromValue value
    DeclarationForeignData data' ->
      Variations.singleLine (Declaration.Foreign.docFromData data')
    DeclarationForeignKind kind -> Declaration.Foreign.docFromKind kind
    DeclarationForeignValue value ->
      Variations.singleLine (Declaration.Foreign.docFromValue value)
    DeclarationNewtype newtype' -> Declaration.DataType.docFromNewtype newtype'
    DeclarationSynonym synonym ->
      Variations.singleLine (Declaration.Synonym.doc synonym)
    DeclarationType declaration ->
      group (flatAlt (Variations.multiLine doc) $ Variations.singleLine doc)
        where
        doc = Type.docFromDeclaration declaration
  static' = \case
    Declarations Nothing -> mempty
    Declarations (Just declarations) ->
      line
        <> intercalateMap1 line staticDoc declarations
  staticDoc = \case
    DeclarationData data' -> Declaration.DataType.docFromData data'
    DeclarationFixityType type' -> Declaration.Fixity.docFromType type'
    DeclarationFixityValue value -> Declaration.Fixity.docFromValue value
    DeclarationForeignData data' ->
      Variations.multiLine (Declaration.Foreign.docFromData data')
    DeclarationForeignKind kind -> Declaration.Foreign.docFromKind kind
    DeclarationForeignValue value ->
      Variations.multiLine (Declaration.Foreign.docFromValue value)
    DeclarationNewtype newtype' -> Declaration.DataType.docFromNewtype newtype'
    DeclarationSynonym synonym ->
      Variations.multiLine (Declaration.Synonym.doc synonym)
    DeclarationType declaration ->
      Variations.multiLine (Type.docFromDeclaration declaration)
