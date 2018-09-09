module Declaration
  ( Declarations(Declarations)
  , dynamic
  , declarations
  , normalize
  , static
  ) where

import "rio" RIO hiding (Data)

import "freer-simple" Control.Monad.Freer        (Eff, Member)
import "freer-simple" Control.Monad.Freer.Error  (Error)
import "semigroupoids" Data.Semigroup.Foldable   (intercalateMap1)
import "prettyprinter" Data.Text.Prettyprint.Doc (Doc, flatAlt, group, line)
import "witherable" Data.Witherable              (wither)
import "base" GHC.Exts                           (fromList)

import qualified "purescript" Language.PureScript

import qualified "this" Annotation
import qualified "this" Declaration.Class
import qualified "this" Declaration.DataType
import qualified "this" Declaration.Fixity
import qualified "this" Declaration.Foreign
import qualified "this" Declaration.Instance
import qualified "this" Declaration.Synonym
import qualified "this" Declaration.Type
import qualified "this" Declaration.Value
import qualified "this" Kind
import qualified "this" List
import qualified "this" Log
import qualified "this" Name
import qualified "this" Type
import qualified "this" Variations

data Declaration a
  = DeclarationClass !(Declaration.Class.Class a)
  | DeclarationData !(Declaration.DataType.Data a)
  | DeclarationFixityType !(Declaration.Fixity.Type a)
  | DeclarationFixityValue !(Declaration.Fixity.Value a)
  | DeclarationForeignData !(Declaration.Foreign.Data a)
  | DeclarationForeignKind !(Declaration.Foreign.Kind a)
  | DeclarationForeignValue !(Declaration.Foreign.Value a)
  | DeclarationInstance !(Declaration.Instance.Instance a)
  | DeclarationNewtype !(Declaration.DataType.Newtype a)
  | DeclarationSynonym !(Declaration.Synonym.Synonym a)
  | DeclarationType !(Declaration.Type.Type a)
  | DeclarationValue !(Declaration.Value.Value a)
  deriving (Functor, Show)

instance (Log.Inspect a) => Log.Inspect (Declaration a)

normalizeDeclaration :: Declaration a -> Declaration Annotation.Normalized
normalizeDeclaration = \case
  DeclarationClass x -> DeclarationClass (Declaration.Class.normalize x)
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
  DeclarationInstance x -> DeclarationInstance (Declaration.Instance.normalize x)
  DeclarationNewtype x ->
    DeclarationNewtype (Declaration.DataType.normalizeNewtype x)
  DeclarationSynonym x -> DeclarationSynonym (Declaration.Synonym.normalize x)
  DeclarationType x -> DeclarationType (Declaration.Type.normalize x)
  DeclarationValue x -> DeclarationValue (Declaration.Value.normalize x)

declarations ::
  ( Member (Error Declaration.Class.InvalidTypeClassMethod) e
  , Member (Error Declaration.Class.MissingTypeVariable) e
  , Member (Error Declaration.DataType.WrongNewtypeConstructors) e
  , Member (Error Declaration.Fixity.NegativePrecedence) e
  , Member (Error Declaration.Instance.DerivedInChain) e
  , Member (Error Declaration.Instance.DerivedNewtypeInChain) e
  , Member (Error Declaration.Instance.Desugared) e
  , Member (Error Declaration.Instance.InvalidMethod) e
  , Member (Error Declaration.Instance.NegativeChainIndex) e
  , Member (Error Declaration.Value.BinaryBinderWithoutOperator) e
  , Member (Error Declaration.Value.CaseAlternativeWithoutBinders) e
  , Member (Error Declaration.Value.CaseAlternativeWithoutExpressions) e
  , Member (Error Declaration.Value.CaseWithoutAlternatives) e
  , Member (Error Declaration.Value.CaseWithoutExpressions) e
  , Member (Error Declaration.Value.DoLetWithoutBindings) e
  , Member (Error Declaration.Value.DoWithoutStatements) e
  , Member (Error Declaration.Value.InvalidExpression) e
  , Member (Error Declaration.Value.InvalidExpressions) e
  , Member (Error Declaration.Value.InvalidLetBinding) e
  , Member (Error Declaration.Value.InvalidWhereDeclaration) e
  , Member (Error Declaration.Value.LetWithoutBindings) e
  , Member (Error Declaration.Value.NoExpressions) e
  , Member (Error Declaration.Value.RecordUpdateWithoutUpdates) e
  , Member (Error Declaration.Value.UnguardedExpression) e
  , Member (Error Declaration.Value.WhereWithoutDeclarations) e
  , Member (Error Kind.InferredKind) e
  , Member (Error Name.InvalidCommon) e
  , Member (Error Name.Missing) e
  , Member (Error Type.InferredConstraintData) e
  , Member (Error Type.InferredForallWithSkolem) e
  , Member (Error Type.InferredSkolem) e
  , Member (Error Type.InferredType) e
  , Member (Error Type.InfixTypeNotTypeOp) e
  , Member (Error Type.PrettyPrintForAll) e
  , Member (Error Type.PrettyPrintFunction) e
  , Member (Error Type.PrettyPrintObject) e
  ) =>
  [Language.PureScript.Declaration] ->
  Eff e (Declarations Annotation.Unannotated)
declarations x = fmap (Declarations . fromList) (wither fromPureScript x)

fromPureScript ::
  ( Member (Error Declaration.Class.InvalidTypeClassMethod) e
  , Member (Error Declaration.Class.MissingTypeVariable) e
  , Member (Error Declaration.DataType.WrongNewtypeConstructors) e
  , Member (Error Declaration.Fixity.NegativePrecedence) e
  , Member (Error Declaration.Instance.DerivedInChain) e
  , Member (Error Declaration.Instance.DerivedNewtypeInChain) e
  , Member (Error Declaration.Instance.Desugared) e
  , Member (Error Declaration.Instance.InvalidMethod) e
  , Member (Error Declaration.Instance.NegativeChainIndex) e
  , Member (Error Declaration.Value.BinaryBinderWithoutOperator) e
  , Member (Error Declaration.Value.CaseAlternativeWithoutBinders) e
  , Member (Error Declaration.Value.CaseAlternativeWithoutExpressions) e
  , Member (Error Declaration.Value.CaseWithoutAlternatives) e
  , Member (Error Declaration.Value.CaseWithoutExpressions) e
  , Member (Error Declaration.Value.DoLetWithoutBindings) e
  , Member (Error Declaration.Value.DoWithoutStatements) e
  , Member (Error Declaration.Value.InvalidExpression) e
  , Member (Error Declaration.Value.InvalidExpressions) e
  , Member (Error Declaration.Value.InvalidLetBinding) e
  , Member (Error Declaration.Value.InvalidWhereDeclaration) e
  , Member (Error Declaration.Value.LetWithoutBindings) e
  , Member (Error Declaration.Value.NoExpressions) e
  , Member (Error Declaration.Value.RecordUpdateWithoutUpdates) e
  , Member (Error Declaration.Value.UnguardedExpression) e
  , Member (Error Declaration.Value.WhereWithoutDeclarations) e
  , Member (Error Kind.InferredKind) e
  , Member (Error Name.InvalidCommon) e
  , Member (Error Name.Missing) e
  , Member (Error Type.InferredConstraintData) e
  , Member (Error Type.InferredForallWithSkolem) e
  , Member (Error Type.InferredSkolem) e
  , Member (Error Type.InferredType) e
  , Member (Error Type.InfixTypeNotTypeOp) e
  , Member (Error Type.PrettyPrintForAll) e
  , Member (Error Type.PrettyPrintFunction) e
  , Member (Error Type.PrettyPrintObject) e
  ) =>
  Language.PureScript.Declaration ->
  Eff e (Maybe (Declaration Annotation.Unannotated))
fromPureScript = \case
  Language.PureScript.BoundValueDeclaration {} -> pure Nothing
  Language.PureScript.DataDeclaration sourceAnn Language.PureScript.Data name variables constructors ->
    Just . DeclarationData <$> Declaration.DataType.data' sourceAnn name variables constructors
  Language.PureScript.DataDeclaration sourceAnn Language.PureScript.Newtype name variables constructors ->
    Just . DeclarationNewtype <$> Declaration.DataType.newtype' sourceAnn name variables constructors
  Language.PureScript.ExternDataDeclaration sourceAnn type' kind ->
    Just . DeclarationForeignData <$> Declaration.Foreign.data' sourceAnn type' kind
  Language.PureScript.ExternDeclaration sourceAnn name type' ->
    Just . DeclarationForeignValue <$> Declaration.Foreign.value sourceAnn name type'
  Language.PureScript.ExternKindDeclaration sourceAnn name ->
    pure (Just $ DeclarationForeignKind $ Declaration.Foreign.kind sourceAnn name)
  Language.PureScript.FixityDeclaration sourceAnn (Left fixity) ->
    Just . DeclarationFixityValue <$> Declaration.Fixity.value sourceAnn fixity
  Language.PureScript.FixityDeclaration sourceAnn (Right fixity) ->
    Just . DeclarationFixityType <$> Declaration.Fixity.type' sourceAnn fixity
  Language.PureScript.TypeClassDeclaration sourceAnn name variables constraints funDeps methods ->
    Just . DeclarationClass <$> Declaration.Class.fromPureScript sourceAnn constraints name variables funDeps methods
  Language.PureScript.TypeDeclaration declaration ->
    Just . DeclarationType <$> Declaration.Type.fromPureScript declaration
  Language.PureScript.TypeInstanceDeclaration sourceAnn _ index instanceName constraints className types body ->
    Just . DeclarationInstance <$> Declaration.Instance.fromPureScript sourceAnn index instanceName constraints className types body
  Language.PureScript.TypeSynonymDeclaration sourceAnn name variables type' ->
    Just . DeclarationSynonym <$> Declaration.Synonym.fromPureScript sourceAnn name variables type'
  Language.PureScript.ValueDeclaration declaration ->
    Just . DeclarationValue <$> Declaration.Value.fromPureScript declaration
  Language.PureScript.BindingGroupDeclaration {} -> pure Nothing
  Language.PureScript.DataBindingGroupDeclaration {} -> pure Nothing
  Language.PureScript.ImportDeclaration {} -> pure Nothing

newtype Declarations a
  = Declarations (List.List (Declaration a))
  deriving (Show)

instance (Log.Inspect a) => Log.Inspect (Declarations a)

normalize :: Declarations a -> Declarations Annotation.Normalized
normalize = \case
  Declarations declarations'' ->
    Declarations (fmap normalizeDeclaration declarations'')

dynamic, static :: Declarations Annotation.Normalized -> Doc a
(dynamic, static) = (dynamic', static')
  where
  dynamic' = \case
    Declarations List.Empty -> mempty
    Declarations (List.NonEmpty declarations') ->
      line
        <> intercalateMap1 line dynamicDoc declarations'
  dynamicDoc = \case
    DeclarationClass class' ->
      Variations.singleLine (Declaration.Class.doc class')
    DeclarationData data' -> Declaration.DataType.docFromData data'
    DeclarationFixityType type' -> Declaration.Fixity.docFromType type'
    DeclarationFixityValue value -> Declaration.Fixity.docFromValue value
    DeclarationForeignData data' ->
      Variations.singleLine (Declaration.Foreign.docFromData data')
    DeclarationForeignKind kind -> Declaration.Foreign.docFromKind kind
    DeclarationForeignValue value ->
      Variations.singleLine (Declaration.Foreign.docFromValue value)
    DeclarationInstance declaration ->
      Declaration.Instance.dynamic declaration
        <> line
    DeclarationNewtype newtype' -> Declaration.DataType.docFromNewtype newtype'
    DeclarationSynonym synonym ->
      Variations.singleLine (Declaration.Synonym.doc synonym)
    DeclarationType declaration ->
      group (flatAlt (Variations.multiLine doc) $ Variations.singleLine doc)
        where
        doc = Declaration.Type.doc declaration
    DeclarationValue declaration ->
      Declaration.Value.dynamic declaration
        <> line
  static' = \case
    Declarations List.Empty -> mempty
    Declarations (List.NonEmpty declarations') ->
      line
        <> intercalateMap1 line staticDoc declarations'
  staticDoc = \case
    DeclarationClass class' ->
      Variations.multiLine (Declaration.Class.doc class')
    DeclarationData data' -> Declaration.DataType.docFromData data'
    DeclarationFixityType type' -> Declaration.Fixity.docFromType type'
    DeclarationFixityValue value -> Declaration.Fixity.docFromValue value
    DeclarationForeignData data' ->
      Variations.multiLine (Declaration.Foreign.docFromData data')
    DeclarationForeignKind kind -> Declaration.Foreign.docFromKind kind
    DeclarationForeignValue value ->
      Variations.multiLine (Declaration.Foreign.docFromValue value)
    DeclarationInstance declaration ->
      Declaration.Instance.static declaration
        <> line
    DeclarationNewtype newtype' -> Declaration.DataType.docFromNewtype newtype'
    DeclarationSynonym synonym ->
      Variations.multiLine (Declaration.Synonym.doc synonym)
    DeclarationType declaration ->
      Variations.multiLine (Declaration.Type.doc declaration)
    DeclarationValue declaration ->
      Declaration.Value.static declaration
        <> line
