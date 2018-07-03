module AST where

import "rio" RIO

import "lens" Control.Lens                     (Prism', prism)
import "lens" Control.Monad.Error.Lens         (throwing_)
import "mtl" Control.Monad.Except              (MonadError)
import "base" Data.List.NonEmpty               (NonEmpty, nonEmpty)
import "semigroupoids" Data.Semigroup.Foldable (intercalateMap1)

import qualified "purescript" Language.PureScript

data Module
  = Module ModuleName

instance Display Module where
  display = \case
    Module name -> "Module name: " <> display name

newtype ModuleName
  = ModuleName (NonEmpty ProperName)

instance Display ModuleName where
  display = \case
    ModuleName names ->
      "ModuleName: [" <> intercalateMap1 ", " display names <> "]"

newtype ProperName
  = ProperName Text

instance Display ProperName where
  display = \case
    ProperName name -> "ProperName: " <> display name

data Error
  = MissingName

instance Display Error where
  display = \case
    MissingName -> "Module missing a name"

class (IsMissingName error) => IsError error where
  _Error :: Prism' error Error

instance IsError Error where
  _Error = prism id Right

class IsMissingName error where
  _MissingName :: Prism' error ()

instance IsMissingName Error where
  _MissingName = prism (const MissingName) $ \case
    MissingName -> Right ()

fromPureScript ::
  (IsMissingName e, MonadError e f) =>
  Language.PureScript.Module ->
  f Module
fromPureScript = \case
  Language.PureScript.Module _ _ name _ _ -> fmap Module (fromModuleName name)

fromModuleName ::
  (IsMissingName e, MonadError e f) =>
  Language.PureScript.ModuleName ->
  f ModuleName
fromModuleName = \case
  Language.PureScript.ModuleName names' ->
    maybe (throwing_ _MissingName) pure $ do
      names <- nonEmpty (fmap fromProperName names')
      pure (ModuleName names)

fromProperName :: Language.PureScript.ProperName a -> ProperName
fromProperName = \case
  Language.PureScript.ProperName name -> ProperName name
