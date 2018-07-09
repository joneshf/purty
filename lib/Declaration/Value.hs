module Declaration.Value where

import "rio" RIO

import "freer-simple" Control.Monad.Freer        (Eff, Members)
import "freer-simple" Control.Monad.Freer.Error  (Error, throwError)
import "base" Data.List                          (intersperse)
import "base" Data.List.NonEmpty                 (NonEmpty, nonEmpty)
import "semigroupoids" Data.Semigroup.Foldable   (intercalateMap1)
import "prettyprinter" Data.Text.Prettyprint.Doc
    ( Doc
    , colon
    , hardline
    , indent
    , lbrace
    , lbracket
    , line
    , parens
    , pretty
    , rbrace
    , rbracket
    , space
    , viaShow
    , (<+>)
    )

import qualified "purescript" Language.PureScript
import qualified "purescript" Language.PureScript.Label
import qualified "purescript" Language.PureScript.PSString

import qualified "this" Annotation
import qualified "this" Comment
import qualified "this" Kind
import qualified "this" Name
import qualified "this" Type
import qualified "this" Variations

data Binder a
  = BinderAs !(Name.Common a) !(Binder a)
  | BinderBinary !(Binder a) !(Name.Qualified Name.ValueOperator a) !(Binder a)
  | BinderCommented !(Binder a) ![Comment.Comment]
  | BinderConstructor
      !(Name.Qualified Name.Constructor a)
      !(Maybe (NonEmpty (Binder a)))
  | BinderLiteral !(Literal (Binder a))
  | BinderOperator !(Name.Qualified Name.ValueOperator a)
  | BinderParens !(Binder a)
  | BinderTyped !(Binder a) !(Type.Type a)
  | BinderVariable !(Name.Common a)
  | BinderWildcard
  deriving (Functor, Show)

binder ::
  ( Members
    '[ Error BinaryBinderWithoutOperator
     , Error NoExpressions
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
  Language.PureScript.Binder ->
  Eff e (Binder Annotation.Unannotated)
binder = \case
  Language.PureScript.BinaryNoParensBinder (Language.PureScript.OpBinder _ x) y z -> do
    left <- binder y
    operator <- Name.qualified (pure . Name.valueOperator) x
    right <- binder z
    pure (BinderBinary left operator right)
  Language.PureScript.BinaryNoParensBinder x y z ->
    throwError (BinaryBinderWithoutOperator y x z)
  Language.PureScript.ConstructorBinder _ x y -> do
    name <- Name.qualified (pure . Name.constructor) x
    binders <- nonEmpty <$> traverse binder y
    pure (BinderConstructor name binders)
  Language.PureScript.LiteralBinder _ x -> fmap BinderLiteral (literal binder x)
  Language.PureScript.NamedBinder _ x y -> do
    name <- Name.common x
    binder' <- binder y
    pure (BinderAs name binder')
  Language.PureScript.NullBinder -> pure BinderWildcard
  Language.PureScript.OpBinder _ x ->
    fmap BinderOperator (Name.qualified (pure . Name.valueOperator) x)
  Language.PureScript.ParensInBinder x -> fmap BinderParens (binder x)
  Language.PureScript.PositionedBinder _ x y -> do
    let comments = fmap Comment.fromPureScript x
    binder' <- binder y
    pure (BinderCommented binder' comments)
  Language.PureScript.TypedBinder x y -> do
    binder' <- binder y
    type' <- Type.fromPureScript x
    pure (BinderTyped binder' type')
  Language.PureScript.VarBinder _ x -> fmap BinderVariable (Name.common x)

docFromBinder :: Binder Annotation.Normalized -> Doc a
docFromBinder = \case
  BinderAs x y -> Name.docFromCommon x <> "@" <> docFromBinder y
  BinderBinary x y z ->
    docFromBinder x
      <+> Name.docFromQualified Name.docFromValueOperator y
      <+> docFromBinder z
  BinderCommented x y -> foldMap Comment.doc y <> docFromBinder x
  BinderConstructor x Nothing -> Name.docFromQualified Name.docFromConstructor x
  BinderConstructor x (Just y) ->
    Name.docFromQualified Name.docFromConstructor x
      <+> intercalateMap1 space docFromBinder y
  BinderLiteral x ->
    Variations.singleLine (docFromLiteral (pure . docFromBinder) x)
  BinderOperator x -> Name.docFromQualified Name.docFromValueOperator x
  BinderParens x -> parens (docFromBinder x)
  BinderTyped x y ->
    docFromBinder x <+> colon <> colon <+> Variations.singleLine (Type.doc y)
  BinderVariable x -> Name.docFromCommon x
  BinderWildcard -> "_"

normalizeBinder :: Binder a -> Binder Annotation.Normalized
normalizeBinder = \case
  BinderAs x y -> BinderAs (Annotation.None <$ x) (normalizeBinder y)
  BinderBinary x y z ->
    BinderBinary (normalizeBinder x) (Annotation.None <$ y) (normalizeBinder z)
  BinderCommented x y -> BinderCommented (normalizeBinder x) y
  BinderConstructor x y ->
    BinderConstructor
      (Annotation.None <$ x)
      ((fmap . fmap) normalizeBinder y)
  BinderLiteral x -> BinderLiteral (fmap normalizeBinder x)
  BinderOperator x -> BinderOperator (Annotation.None <$ x)
  BinderParens x -> BinderParens (normalizeBinder x)
  BinderTyped x y -> BinderTyped (normalizeBinder x) (Type.normalize y)
  BinderVariable x -> BinderVariable (Annotation.None <$ x)
  BinderWildcard -> BinderWildcard

data Literal a
  = LiteralArray !(Maybe (NonEmpty a))
  | LiteralBoolean !Bool
  | LiteralChar !Char
  | LiteralInt !Integer
  | LiteralNumber !Double
  | LiteralRecord !(Maybe (NonEmpty (RecordPair a)))
  | LiteralString !Language.PureScript.PSString.PSString
  deriving (Functor, Show)

docFromLiteral ::
  (a -> Variations.Variations (Doc b)) ->
  Literal a ->
  Variations.Variations (Doc b)
docFromLiteral f = \case
  LiteralArray Nothing -> pure (lbracket <> rbracket)
  LiteralArray (Just x) -> Variations.bracketesize f x
  LiteralBoolean True -> pure "true"
  LiteralBoolean False -> pure "false"
  LiteralChar x -> pure (viaShow x)
  LiteralInt x -> pure (pretty x)
  LiteralNumber x -> pure (pretty x)
  LiteralRecord Nothing -> pure (lbrace <> rbrace)
  LiteralRecord (Just x) -> Variations.bracesize (docFromRecordPair f) x
  LiteralString x -> pure (pretty $ Language.PureScript.prettyPrintString x)

literal :: (a -> Eff e b) -> Language.PureScript.Literal a -> Eff e (Literal b)
literal f = \case
  Language.PureScript.ArrayLiteral x -> LiteralArray . nonEmpty <$> traverse f x
  Language.PureScript.BooleanLiteral x -> pure (LiteralBoolean x)
  Language.PureScript.CharLiteral x -> pure (LiteralChar x)
  Language.PureScript.NumericLiteral (Left x) -> pure (LiteralInt x)
  Language.PureScript.NumericLiteral (Right x) -> pure (LiteralNumber x)
  Language.PureScript.ObjectLiteral x ->
    LiteralRecord . nonEmpty <$> traverse (recordPair f) x
  Language.PureScript.StringLiteral x -> pure (LiteralString x)

data RecordPair a
  = RecordPair !Language.PureScript.Label.Label !a
  deriving (Functor, Show)

docFromRecordPair ::
  (a -> Variations.Variations (Doc b)) ->
  RecordPair a ->
  Variations.Variations (Doc b)
docFromRecordPair f = \case
  RecordPair x y ->
    Variations.Variations { Variations.multiLine, Variations.singleLine }
      where
      multiLine =
        pretty (Language.PureScript.prettyPrintLabel x)
          <> colon
          <> line
          <> indent 2 (Variations.singleLine $ f y)
      singleLine =
        pretty (Language.PureScript.prettyPrintLabel x)
          <> colon
          <+> Variations.singleLine (f y)

recordPair ::
  (a -> Eff e b) ->
  (Language.PureScript.PSString.PSString, a) ->
  Eff e (RecordPair b)
recordPair f = \case
  (x, y) -> fmap (RecordPair $ Language.PureScript.Label.Label x) (f y)

data Value a
  = Value !(Name.Common a) !(Maybe (NonEmpty (Binder a)))
  deriving (Functor, Show)

doc :: Value Annotation.Normalized -> Variations.Variations (Doc a)
doc = \case
  Value x y ->
    Variations.Variations { Variations.multiLine, Variations.singleLine }
      where
      bindersDoc binders =
        space <> intercalateMap1 space docFromBinder binders
      multiLine =
        Name.docFromCommon x
          <> foldMap bindersDoc y
          <> hardline
      singleLine =
        Name.docFromCommon x
          <> foldMap bindersDoc y
          <> hardline

fromPureScript ::
  ( Members
    '[ Error BinaryBinderWithoutOperator
     , Error NoExpressions
     , Error Name.InvalidCommon
     , Error Name.Missing
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
  Language.PureScript.ValueDeclarationData [Language.PureScript.GuardedExpr] ->
  Eff e (Value Annotation.Unannotated)
fromPureScript = \case
  Language.PureScript.ValueDeclarationData _ name _ _ [] ->
    throwError (NoExpressions name)
  Language.PureScript.ValueDeclarationData _ name' _ binders' _ -> do
    name <- Name.common name'
    binders <- nonEmpty <$> traverse binder binders'
    pure (Value name binders)

normalize :: Value a -> Value Annotation.Normalized
normalize = \case
  Value name binders ->
    Value
      (Annotation.None <$ name)
      ((fmap . fmap) normalizeBinder binders)

displayList :: Display a => [a] -> Utf8Builder
displayList xs = "[" <> fold (intersperse ", " (display <$> xs)) <> "]"

displayNonEmpty :: (Display a) => Maybe (NonEmpty a) -> Utf8Builder
displayNonEmpty xs = "[" <> foldMap (intercalateMap1 ", " display) xs <> "]"

-- Errors

type Errors
  = '[ Error BinaryBinderWithoutOperator
     , Error NoExpressions
     ]

data BinaryBinderWithoutOperator
  = BinaryBinderWithoutOperator
      !Language.PureScript.Binder
      !Language.PureScript.Binder
      !Language.PureScript.Binder

instance Display BinaryBinderWithoutOperator where
  display = \case
    BinaryBinderWithoutOperator x y z ->
      "We received a binary binder with `"
        <> displayShow y
        <> "` as the operator."
        <> " The left side was `"
        <> displayShow x
        <> "`. The right side was `"
        <> displayShow z
        <> "`."
        <> " If there is an operator nested within, we should handle that case."
        <> " Otherwise, this is probably a problem in the PureScript library."

newtype NoExpressions
  = NoExpressions Language.PureScript.Ident

instance Display NoExpressions where
  display = \case
    NoExpressions x ->
      "We recieved a value `"
        <> displayShow x
        <> "` that had no expressions."
        <> " This is most likely a problem with the PureScript library."
