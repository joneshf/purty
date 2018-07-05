module Name where

import "rio" RIO

import "freer-simple" Control.Monad.Freer        (Eff, Members)
import "freer-simple" Control.Monad.Freer.Error  (Error, throwError)
import "base" Data.List.NonEmpty                 (NonEmpty, nonEmpty)
import "semigroupoids" Data.Semigroup.Foldable   (intercalateMap1)
import "prettyprinter" Data.Text.Prettyprint.Doc (Doc, dot, pretty)

import qualified "purescript" Language.PureScript

import qualified "this" Annotation

data Common a
  = Common !a !Text
  deriving (Eq, Functor, Ord)

instance (Display a) => Display (Common a) where
  display = \case
    Common ann x ->
      "Common annotation:"
      <> display ann
      <> ", common: "
      <> display x

docFromCommon :: Common a -> Doc b
docFromCommon = \case
  Common _ann name -> pretty name

common ::
  (Members '[Error InvalidCommon] e) =>
  Language.PureScript.Ident ->
  Eff e (Common Annotation.Unannotated)
common = \case
  Language.PureScript.Ident ident -> pure (Common Annotation.Unannotated ident)
  ident -> throwError (InvalidCommon ident)

newtype Constructor a
  = Constructor (Proper a)
  deriving (Functor)

instance (Display a) => Display (Constructor a) where
  display = \case
    Constructor name -> "Constructor: " <> display name

constructor ::
  Language.PureScript.ProperName 'Language.PureScript.ConstructorName ->
  Constructor Annotation.Unannotated
constructor = Constructor . proper

docFromConstructor :: Constructor a -> Doc b
docFromConstructor = \case
  Constructor name -> docFromProper name

newtype Class a
  = Class (Proper a)
  deriving (Eq, Functor, Ord)

instance (Display a) => Display (Class a) where
  display = \case
    Class name -> "Class: " <> display name

class' ::
  Language.PureScript.ProperName 'Language.PureScript.ClassName ->
  Class Annotation.Unannotated
class' = Class . proper

docFromClass :: Class a -> Doc b
docFromClass = \case
  Class name -> docFromProper name

newtype Kind a
  = Kind (Proper a)
  deriving (Eq, Functor, Ord)

instance (Display a) => Display (Kind a) where
  display = \case
    Kind name -> "Kind: " <> display name

docFromKind :: Kind a -> Doc b
docFromKind = \case
  Kind name -> docFromProper name

kind ::
  Language.PureScript.ProperName 'Language.PureScript.KindName ->
  Kind Annotation.Unannotated
kind = Kind . proper

newtype Module a
  = Module (NonEmpty (Proper a))
  deriving (Eq, Functor, Ord)

instance (Display a) => Display (Module a) where
  display = \case
    Module names ->
      "Module: [" <> intercalateMap1 ", " display names <> "]"

docFromModule :: Module a -> Doc b
docFromModule = \case
  Module names -> intercalateMap1 "." docFromProper names

module' ::
  (Members '[Error Missing] e) =>
  Language.PureScript.ModuleName ->
  Eff e (Module Annotation.Unannotated)
module' = \case
  Language.PureScript.ModuleName names ->
    maybe (throwError Missing) pure (fmap Module $ nonEmpty $ fmap proper names)

data Qualified f a
  = Qualified !(Maybe (Module a)) !(f a)
  deriving (Functor)

instance (Display a, Display (f a)) => Display (Qualified f a) where
  display = \case
    Qualified Nothing x ->
      "UnQualified: "
        <> "qualified: "
        <> display x
    Qualified (Just x) y ->
      "Qualified: "
        <> "module: "
        <> display x
        <> ", qualified: "
        <> display y

docFromQualified :: (f a -> Doc b) -> Qualified f a -> Doc b
docFromQualified f = \case
  Qualified Nothing x -> f x
  Qualified (Just x) y -> docFromModule x <> dot <> f y

qualified ::
  (Members '[Error Missing] e) =>
  (a -> Eff e (g Annotation.Unannotated)) ->
  Language.PureScript.Qualified a ->
  Eff e (Qualified g Annotation.Unannotated)
qualified f = \case
  Language.PureScript.Qualified m x -> do
    module'' <- traverse module' m
    y <- f x
    pure (Qualified module'' y)

data Proper a
  = Proper !a !Text
  deriving (Eq, Functor, Ord)

instance (Display a) => Display (Proper a) where
  display = \case
    Proper ann name ->
      "Proper annotation: "
        <> display ann
        <> ", name: "
        <> display name

compareProper :: Proper a -> Proper b -> Ordering
compareProper x' y' = case (x', y') of
  (Proper _ x, Proper _ y) -> compare x y

proper :: Language.PureScript.ProperName a -> Proper Annotation.Unannotated
proper = \case
  Language.PureScript.ProperName name -> Proper Annotation.Unannotated name

docFromProper :: Proper a -> Doc b
docFromProper = \case
  Proper _ann name -> pretty name

newtype Type a
  = Type (Proper a)
  deriving (Eq, Functor, Ord)

instance (Display a) => Display (Type a) where
  display = \case
    Type name -> "Type: " <> display name

docFromType :: Type a -> Doc b
docFromType = \case
  Type name -> docFromProper name

type' ::
  Language.PureScript.ProperName 'Language.PureScript.TypeName ->
  Type Annotation.Unannotated
type' = Type . proper

newtype TypeConstructor a
  = TypeConstructor (Proper a)
  deriving (Functor)

instance (Display a) => Display (TypeConstructor a) where
  display = \case
    TypeConstructor name -> "TypeConstructor: " <> display name

docFromTypeConstructor :: TypeConstructor a -> Doc b
docFromTypeConstructor = \case
  TypeConstructor name -> docFromProper name

typeConstructor ::
  Language.PureScript.ProperName 'Language.PureScript.TypeName ->
  TypeConstructor Annotation.Unannotated
typeConstructor = TypeConstructor . proper

data TypeOperator a
  = TypeOperator !a !Text
  deriving (Functor)

instance (Display a) => Display (TypeOperator a) where
  display = \case
    TypeOperator ann op ->
      "Type Operator: "
        <> "annotation: "
        <> display ann
        <> ", operator: "
        <> display op

docFromTypeOperator :: TypeOperator a -> Doc b
docFromTypeOperator = \case
  TypeOperator _ann op -> pretty op

typeOperator ::
  Language.PureScript.OpName 'Language.PureScript.TypeOpName ->
  TypeOperator Annotation.Unannotated
typeOperator = \case
  Language.PureScript.OpName x -> TypeOperator Annotation.Unannotated x

-- Errors

type Errors
  = '[ Error InvalidCommon
     , Error Missing
     ]

newtype InvalidCommon
  = InvalidCommon Language.PureScript.Ident

instance Display InvalidCommon where
  display = \case
    InvalidCommon ident ->
      "Invalid common name: " <> displayShow ident

data Missing
  = Missing

instance Display Missing where
  display = \case
    Missing -> "Module missing a name"
