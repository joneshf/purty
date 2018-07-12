module Declaration.Instance where

import "rio" RIO

import "freer-simple" Control.Monad.Freer        (Eff, Members)
import "freer-simple" Control.Monad.Freer.Error  (Error, throwError)
import "base" Data.List.NonEmpty                 (NonEmpty, nonEmpty)
import "semigroupoids" Data.Semigroup.Foldable   (intercalateMap1)
import "prettyprinter" Data.Text.Prettyprint.Doc
    ( Doc
    , align
    , colon
    , flatAlt
    , group
    , indent
    , line
    , space
    , (<+>)
    )

import qualified "purescript" Language.PureScript

import qualified "this" Annotation
import qualified "this" Declaration.Type
import qualified "this" Declaration.Value
import qualified "this" Kind
import qualified "this" Name
import qualified "this" Type
import qualified "this" Variations

data Instance a
  = Instance
      !(Type a)
      !(Name.Common a)
      !(Maybe (NonEmpty (Type.Constraint a)))
      !(Name.Qualified Name.Class a)
      !(Maybe (NonEmpty (Type.Type a)))
  deriving (Functor, Show)

dynamic :: Instance Annotation.Normalized -> Doc a
dynamic = \case
  Instance type' instanceName constraints className types -> case type' of
    TypeDerived              -> "derive instance" <+> doc mempty
    TypeExplicit methods     -> "instance" <+> doc (methodsDoc methods)
    TypeExplicitElse methods -> "else instance" <+> doc (methodsDoc methods)
    TypeNewtype              -> "derive newtype instance" <+> doc mempty
    where
    constraintsDoc = foldMap $ \x ->
      let Variations.Variations {Variations.multiLine, Variations.singleLine} =
            Variations.parenthesize Type.docFromConstraint x
          multi = line <> indent 2 multiLine <+> "=>" <> line <> space
          single = space <> singleLine <+> "=>"
      in group (flatAlt multi single)
    doc rest =
      Name.docFromCommon instanceName
        <+> colon <> colon
        <> constraintsDoc constraints
        <+> Name.docFromQualified Name.docFromClass className
        <> typesDoc types
        <> rest
    methodsDoc = foldMap $ \x ->
      space
        <> "where"
        <> line
        <> indent 2 (align $ intercalateMap1 line dynamicMethod x)
    typesDoc = foldMap $ \x ->
      space <> intercalateMap1 space (Variations.singleLine . Type.doc) x

fromPureScript ::
  ( Members
    '[ Error Desugared
     , Error DerivedInChain
     , Error DerivedNewtypeInChain
     , Error Desugared
     , Error InvalidMethod
     , Error NegativeChainIndex
     , Error Declaration.Value.BinaryBinderWithoutOperator
     , Error Declaration.Value.CaseAlternativeWithoutBinders
     , Error Declaration.Value.CaseAlternativeWithoutExpressions
     , Error Declaration.Value.CaseWithoutAlternatives
     , Error Declaration.Value.CaseWithoutExpressions
     , Error Declaration.Value.DoLetWithoutBindings
     , Error Declaration.Value.DoWithoutStatements
     , Error Declaration.Value.InvalidExpression
     , Error Declaration.Value.InvalidExpressions
     , Error Declaration.Value.InvalidLetBinding
     , Error Declaration.Value.InvalidWhereDeclaration
     , Error Declaration.Value.LetWithoutBindings
     , Error Declaration.Value.NoExpressions
     , Error Declaration.Value.NotImplemented
     , Error Declaration.Value.RecordUpdateWithoutUpdates
     , Error Declaration.Value.UnguardedExpression
     , Error Declaration.Value.WhereWithoutDeclarations
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
  Integer ->
  Language.PureScript.Ident ->
  [Language.PureScript.Constraint] ->
  Language.PureScript.Qualified
    (Language.PureScript.ProperName 'Language.PureScript.ClassName) ->
  [Language.PureScript.Type] ->
  Language.PureScript.TypeInstanceBody ->
  Eff e (Instance Annotation.Unannotated)
fromPureScript index instanceName' constraints' className' types' body
  | index < 0 = throwError (NegativeChainIndex index instanceName')
  | index /= 0
  , Language.PureScript.DerivedInstance <- body = throwError DerivedInChain
  | index /= 0
  , Language.PureScript.NewtypeInstance <- body =
      throwError DerivedNewtypeInChain
  | otherwise = do
    instanceName <- Name.common instanceName'
    constraints <- nonEmpty <$> traverse Type.constraint constraints'
    className <- Name.qualified (pure . Name.class') className'
    types <- nonEmpty <$> traverse Type.fromPureScript types'
    case body of
      Language.PureScript.NewtypeInstanceWithDictionary x ->
        throwError (Desugared x)
      Language.PureScript.DerivedInstance ->
        pure (Instance TypeDerived instanceName constraints className types)
      Language.PureScript.NewtypeInstance ->
        pure (Instance TypeNewtype instanceName constraints className types)
      Language.PureScript.ExplicitInstance x -> do
        methods <- nonEmpty <$> traverse method x
        let type' = case index of
                      0 -> TypeExplicit methods
                      _ -> TypeExplicitElse methods
        pure (Instance type' instanceName constraints className types)

normalize :: Instance a -> Instance Annotation.Normalized
normalize = \case
  Instance type' w x y z ->
    Instance
      (normalizeType type')
      (Annotation.None <$ w)
      ((fmap . fmap) Type.normalizeConstraint x)
      (Annotation.None <$ y)
      ((fmap . fmap) Type.normalize z)

static :: Instance Annotation.Normalized -> Doc a
static = \case
  Instance type' instanceName constraints className types -> case type' of
    TypeDerived              -> "derive instance" <+> doc mempty
    TypeExplicit methods     -> "instance" <+> doc (methodsDoc methods)
    TypeExplicitElse methods -> "else instance" <+> doc (methodsDoc methods)
    TypeNewtype              -> "derive newtype instance" <+> doc mempty
    where
    constraintsDoc = foldMap $ \x ->
      Variations.multiLine (Variations.parenthesize Type.docFromConstraint x)
        <+> "=>"
        <> line
    doc rest =
      Name.docFromCommon instanceName
        <+> colon <> colon
        <> line
        <> indent
          2
          ( constraintsDoc constraints
          <> Name.docFromQualified Name.docFromClass className
          <> typesDoc types
          <> rest
          )
    methodsDoc = foldMap $ \x ->
      space
        <> "where"
        <> line
        <> indent 2 (align $ intercalateMap1 line staticMethod x)
    typesDoc = foldMap $ \x ->
      space <> intercalateMap1 space (Variations.multiLine . Type.doc) x

data Method a
  = MethodType !(Declaration.Type.Type a)
  | MethodValue !(Declaration.Value.Value a)
  deriving (Functor, Show)

dynamicMethod :: Method Annotation.Normalized -> Doc a
dynamicMethod = \case
  MethodType x -> group (flatAlt multiLine singleLine)
    where
    Variations.Variations { Variations.multiLine, Variations.singleLine } =
      Declaration.Type.doc x
  MethodValue x -> Declaration.Value.dynamic x

method ::
  ( Members
    '[ Error InvalidMethod
     , Error Declaration.Value.BinaryBinderWithoutOperator
     , Error Declaration.Value.CaseAlternativeWithoutBinders
     , Error Declaration.Value.CaseAlternativeWithoutExpressions
     , Error Declaration.Value.CaseWithoutAlternatives
     , Error Declaration.Value.CaseWithoutExpressions
     , Error Declaration.Value.DoLetWithoutBindings
     , Error Declaration.Value.DoWithoutStatements
     , Error Declaration.Value.InvalidExpression
     , Error Declaration.Value.InvalidExpressions
     , Error Declaration.Value.InvalidLetBinding
     , Error Declaration.Value.InvalidWhereDeclaration
     , Error Declaration.Value.LetWithoutBindings
     , Error Declaration.Value.NoExpressions
     , Error Declaration.Value.NotImplemented
     , Error Declaration.Value.RecordUpdateWithoutUpdates
     , Error Declaration.Value.UnguardedExpression
     , Error Declaration.Value.WhereWithoutDeclarations
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
  Eff e (Method Annotation.Unannotated)
method = \case
  Language.PureScript.TypeDeclaration x ->
    fmap MethodType (Declaration.Type.fromPureScript x)
  Language.PureScript.ValueDeclaration x ->
    fmap MethodValue (Declaration.Value.fromPureScript x)
  x -> throwError (InvalidMethod x)

normalizeMethod :: Method a -> Method Annotation.Normalized
normalizeMethod = \case
  MethodType x -> MethodType (Declaration.Type.normalize x)
  MethodValue x -> MethodValue (Declaration.Value.normalize x)

staticMethod :: Method Annotation.Normalized -> Doc a
staticMethod = \case
  MethodType x -> Variations.multiLine (Declaration.Type.doc x)
  MethodValue x -> Declaration.Value.static x

data Type a
  = TypeDerived
  | TypeExplicit !(Maybe (NonEmpty (Method a)))
  | TypeExplicitElse !(Maybe (NonEmpty (Method a)))
  | TypeNewtype
  deriving (Functor, Show)

normalizeType :: Type a -> Type Annotation.Normalized
normalizeType = \case
  TypeDerived -> TypeDerived
  TypeExplicit x -> TypeExplicit ((fmap . fmap) normalizeMethod x)
  TypeExplicitElse x -> TypeExplicitElse ((fmap . fmap) normalizeMethod x)
  TypeNewtype -> TypeNewtype

-- Errors

type Errors
  = '[ Error DerivedInChain
     , Error DerivedNewtypeInChain
     , Error Desugared
     , Error InvalidMethod
     , Error NegativeChainIndex
     ]

data DerivedInChain
  = DerivedInChain

instance Display DerivedInChain where
  display = \case
    DerivedInChain ->
      "We received a derived instance in an instance chain,"
        <> " but only explicit instances are allowed in an instance chain."
        <> " We eiither have a problem with interpreting instances,"
        <> " or this is a problem in the PureScript library."

data DerivedNewtypeInChain
  = DerivedNewtypeInChain

instance Display DerivedNewtypeInChain where
  display = \case
    DerivedNewtypeInChain ->
      "We received a derived newtype instance in an instance chain,"
        <> " but only explicit instances are allowed in an instance chain."
        <> " We eiither have a problem with interpreting instances,"
        <> " or this is a problem in the PureScript library."

newtype Desugared
  = Desugared Language.PureScript.Expr

instance Display Desugared where
  display = \case
    Desugared x ->
      "We received an instance that was desugared by the PureScript library,"
        <> " It has the expression `"
        <> displayShow x
        <> "`. This is most likely a problem in the PureScript library."

newtype InvalidMethod
  = InvalidMethod Language.PureScript.Declaration

instance Display InvalidMethod where
  display = \case
    InvalidMethod x ->
      "We received an instance containing a declaration `"
        <> displayShow x
        <> "`, but this is neither a type declaration nor a value declaration."
        <> " This is most likely a problem in the PureScript library."

data NegativeChainIndex
  = NegativeChainIndex !Integer !Language.PureScript.Ident

instance Display NegativeChainIndex where
  display = \case
    NegativeChainIndex x y ->
      "We received an instance `"
        <> displayShow y
        <> "` with a negative chain index `"
        <> displayShow x
        <> "`. This is most likely a problem in the PureScript library."
