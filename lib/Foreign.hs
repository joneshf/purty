module Foreign where

import "rio" RIO hiding (Data)

import "prettyprinter" Data.Text.Prettyprint.Doc (Doc, colon, (<+>))

import qualified "this" Annotation
import qualified "this" Kind
import qualified "this" Name
import qualified "this" Type

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

docFromData :: Data Annotation.Normalized -> Doc a
docFromData = \case
  Data name kind ->
    "foreign import data" <+> Name.docFromType name
      <+> colon <> colon
      <+> Kind.doc kind

normalizeData :: Data a -> Data Annotation.Normalized
normalizeData = \case
  Data type' kind -> Data (Annotation.None <$ type') (Kind.normalize kind)

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
  Foreign.Kind name ->
    "foreign import kind" <+> Name.docFromKind name

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

docFromValue :: Value Annotation.Normalized -> Doc a
docFromValue = \case
  Foreign.Value name type' ->
    "foreign import" <+> Name.docFromCommon name
      <+> colon <> colon
      <+> Type.doc type'

normalizeValue :: Value a -> Value Annotation.Normalized
normalizeValue = \case
  Value name type' -> Value (Annotation.None <$ name) (Type.normalize type')
