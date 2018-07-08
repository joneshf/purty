module Declaration.Foreign where

import "rio" RIO hiding (Data)

import "freer-simple" Control.Monad.Freer        (Eff, Members)
import "freer-simple" Control.Monad.Freer.Error  (Error)
import "prettyprinter" Data.Text.Prettyprint.Doc
    ( Doc
    , colon
    , indent
    , line
    , (<+>)
    )

import qualified "purescript" Language.PureScript

import qualified "this" Annotation
import qualified "this" Kind
import qualified "this" Name
import qualified "this" Type
import qualified "this" Variations

data Data a
  = Data !(Name.Type a) !(Kind.Kind a)
  deriving (Functor)

instance (Display a) => Display (Data a) where
  display = \case
    Data x y ->
      "Data: "
        <> "name: "
        <> display x
        <> ", kind: "
        <> display y

data' ::
  ( Members
    '[ Error Kind.InferredKind
     , Error Name.Missing
     ]
    e
  ) =>
  Language.PureScript.ProperName 'Language.PureScript.TypeName ->
  Language.PureScript.Kind ->
  Eff e (Data Annotation.Unannotated)
data' type' kind' = Data (Name.type' type') <$> Kind.fromPureScript kind'

docFromData :: Data Annotation.Normalized -> Variations.Variations (Doc a)
docFromData = \case
  Data name kind' ->
    Variations.Variations { Variations.multiLine, Variations.singleLine }
      where
      multiLine =
        "foreign import data" <+> Name.docFromType name
          <+> colon <> colon
          <> line
          <> indent 2 (Variations.multiLine $ Kind.doc kind')
          <> line
      singleLine =
        "foreign import data" <+> Name.docFromType name
          <+> colon <> colon
          <+> Variations.singleLine (Kind.doc kind')
          <> line

normalizeData :: Data a -> Data Annotation.Normalized
normalizeData = \case
  Data type' kind' -> Data (Annotation.None <$ type') (Kind.normalize kind')

newtype Kind a
  = Kind (Name.Kind a)
  deriving (Functor)

instance (Display a) => Display (Kind a) where
  display = \case
    Kind x ->
      "Kind: "
        <> "name: "
        <> display x

docFromKind :: Kind Annotation.Normalized -> Doc a
docFromKind = \case
  Kind name ->
    "foreign import kind" <+> Name.docFromKind name
      <> line

kind ::
  Language.PureScript.ProperName 'Language.PureScript.KindName ->
  Kind Annotation.Unannotated
kind = Kind . Name.kind

normalizeKind :: Kind a -> Kind Annotation.Normalized
normalizeKind = \case
  Kind name -> Kind (Annotation.None <$ name)

data Value a
  = Value !(Name.Common a) !(Type.Type a)
  deriving (Functor)

instance (Display a) => Display (Value a) where
  display = \case
    Value x y ->
      "Value: "
        <> "name: "
        <> display x
        <> ", type: "
        <> display y

docFromValue :: Value Annotation.Normalized -> Variations.Variations (Doc a)
docFromValue = \case
  Value name type' ->
    Variations.Variations { Variations.multiLine, Variations.singleLine }
      where
      multiLine =
        "foreign import" <+> Name.docFromCommon name
          <+> colon <> colon
          <> line
          <> indent 2 (Variations.multiLine $ Type.doc type')
          <> line
      singleLine =
        "foreign import" <+> Name.docFromCommon name
          <+> colon <> colon <+> Variations.singleLine (Type.doc type')
          <> line

normalizeValue :: Value a -> Value Annotation.Normalized
normalizeValue = \case
  Value name type' -> Value (Annotation.None <$ name) (Type.normalize type')

value ::
  ( Members
    '[ Error Kind.InferredKind
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
  Language.PureScript.Ident ->
  Language.PureScript.Type ->
  Eff e (Value Annotation.Unannotated)
value name' type'' = do
  name <- Name.common name'
  type' <- Type.fromPureScript type''
  pure (Value name type')
