module AST where

import "rio" RIO

import "lens" Control.Lens                     (Prism', prism)
import "lens" Control.Monad.Error.Lens         (throwing, throwing_)
import "mtl" Control.Monad.Except              (MonadError)
import "base" Data.List.NonEmpty               (NonEmpty, nonEmpty)
import "semigroupoids" Data.Semigroup.Foldable (intercalateMap1)

import qualified "purescript" Language.PureScript

data Module
  = Module ModuleName (Maybe (NonEmpty Export))

instance Display Module where
  display = \case
    Module name exports ->
      "{Module name: "
        <> display name
        <> foldMap (\x -> ", exports: " <> intercalateMap1 ", " display x) exports
        <> "}"

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

data Export
  = ExportValue Ident

instance Display Export where
  display = \case
    ExportValue ident -> "Export value: " <> display ident

newtype Ident
  = Ident Text

instance Display Ident where
  display = \case
    Ident x -> "Ident: " <> display x

data Error
  = EmptyExplicitExports
  | InvalidExport Language.PureScript.Ident
  | MissingName

instance Display Error where
  display = \case
    EmptyExplicitExports -> "Module has an empty export list"
    InvalidExport ident ->
      "Module exports an invalid identifier: " <> displayShow ident
    MissingName -> "Module missing a name"

class
  ( IsEmptyExplicitExports error
  , IsInvalidExport error
  , IsMissingName error
  ) =>
  IsError error where
    _Error :: Prism' error Error

instance IsError Error where
  _Error = prism id Right

class IsEmptyExplicitExports error where
  _EmptyExplicitExports :: Prism' error ()

instance IsEmptyExplicitExports Error where
  _EmptyExplicitExports = prism (const EmptyExplicitExports) $ \case
    EmptyExplicitExports -> Right ()
    x -> Left x

class IsInvalidExport error where
  _InvalidExport :: Prism' error Language.PureScript.Ident

instance IsInvalidExport Error where
  _InvalidExport = prism InvalidExport $ \case
    InvalidExport ident -> Right ident
    x -> Left x

class IsMissingName error where
  _MissingName :: Prism' error ()

instance IsMissingName Error where
  _MissingName = prism (const MissingName) $ \case
    MissingName -> Right ()
    x -> Left x

newtype NotImplemented
  = NotImplemented Utf8Builder

instance Display NotImplemented where
  display = \case
    NotImplemented x -> "We have not yet implemented: " <> x

class IsNotImplemented error where
  _NotImplemented :: Prism' error Utf8Builder

instance IsNotImplemented NotImplemented where
  _NotImplemented = prism NotImplemented $ \case
    NotImplemented x -> Right x

fromExport ::
  (IsInvalidExport e, IsNotImplemented e, MonadError e f) =>
  Language.PureScript.DeclarationRef ->
  f Export
fromExport = \case
  Language.PureScript.ValueRef _ ident -> fmap ExportValue (fromIdent ident)
  ref -> throwing _NotImplemented (displayShow ref)

fromExports ::
  (IsEmptyExplicitExports e, IsInvalidExport e, IsNotImplemented e, MonadError e f) =>
  [Language.PureScript.DeclarationRef] ->
  f (NonEmpty Export)
fromExports =
  maybe (throwing_ _EmptyExplicitExports) (traverse fromExport) . nonEmpty

fromIdent ::
  (IsInvalidExport e, MonadError e f) =>
  Language.PureScript.Ident ->
  f Ident
fromIdent = \case
  Language.PureScript.Ident ident -> pure (Ident ident)
  ident -> throwing _InvalidExport ident

fromPureScript ::
  (IsError e, IsNotImplemented e, MonadError e f) =>
  Language.PureScript.Module ->
  f Module
fromPureScript = \case
  Language.PureScript.Module _ _ name' _ exports' -> do
    name <- fromModuleName name'
    exports <- traverse fromExports exports'
    pure (Module name exports)

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
