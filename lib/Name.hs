module Name where

import "rio" RIO

import "lens" Control.Lens                       (Prism', prism)
import "lens" Control.Monad.Error.Lens           (throwing_)
import "mtl" Control.Monad.Except                (MonadError)
import "base" Data.List.NonEmpty                 (NonEmpty, nonEmpty)
import "semigroupoids" Data.Semigroup.Foldable   (intercalateMap1)
import "prettyprinter" Data.Text.Prettyprint.Doc (Doc, pretty)

import qualified "purescript" Language.PureScript

import qualified "this" Annotation

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
  (IsMissing e, MonadError e f) =>
  Language.PureScript.ModuleName ->
  f (Module Annotation.Unannotated)
module' = \case
  Language.PureScript.ModuleName names' ->
    maybe (throwing_ _Missing) pure $ do
      names <- nonEmpty (fmap Name.proper names')
      pure (Module names)

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

-- Errors

data Error
  = Missing

instance Display Error where
  display = \case
    Missing -> "Module missing a name"

class (IsMissing error) => IsError error where
    _Error :: Prism' error Error

instance IsError Error where
  _Error = prism id Right

class IsMissing error where
  _Missing :: Prism' error ()

instance IsMissing Error where
  _Missing = prism (const Missing) $ \case
    Missing -> Right ()
