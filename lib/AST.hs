module AST where

import "rio" RIO

import "freer-simple" Control.Monad.Freer       (Eff, Members)
import "freer-simple" Control.Monad.Freer.Error (throwError)
import "base" Data.List.NonEmpty                (NonEmpty, nonEmpty)
import "semigroupoids" Data.Semigroup.Foldable  (intercalateMap1)

import qualified "freer-simple" Control.Monad.Freer.Error
import qualified "purescript" Language.PureScript

newtype Module
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

fromPureScript ::
  (Members '[Control.Monad.Freer.Error.Error Error] e) =>
  Language.PureScript.Module ->
  Eff e Module
fromPureScript = \case
  Language.PureScript.Module _ _ name _ _ -> fmap Module (fromModuleName name)

fromModuleName ::
  (Members '[Control.Monad.Freer.Error.Error Error] e) =>
  Language.PureScript.ModuleName ->
  Eff e ModuleName
fromModuleName = \case
  Language.PureScript.ModuleName names' ->
    maybe
      (throwError MissingName)
      pure
      (fmap ModuleName $ nonEmpty $ fmap fromProperName names')

fromProperName :: Language.PureScript.ProperName a -> ProperName
fromProperName = \case
  Language.PureScript.ProperName name -> ProperName name
