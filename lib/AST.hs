module AST where

import "rio" RIO

import "lens" Control.Lens                     (Prism', prism)
import "lens" Control.Monad.Error.Lens         (throwing, throwing_)
import "mtl" Control.Monad.Except              (MonadError)
import "base" Data.List.NonEmpty               (NonEmpty, nonEmpty, sortBy)
import "semigroupoids" Data.Semigroup.Foldable (intercalateMap1)

import qualified "purescript" Language.PureScript

data Module a
  = Module !a !(ModuleName a) !(Maybe (NonEmpty (Export a)))
  deriving (Functor)

instance (Display a) => Display (Module a) where
  display = \case
    Module ann name exports ->
      "{Module "
        <> "annotation: "
        <> display ann
        <> ", name: "
        <> display name
        <> foldMap (\x -> ", exports: " <> intercalateMap1 ", " display x) exports
        <> "}"

newtype ModuleName a
  = ModuleName (NonEmpty (ProperName a))
  deriving (Eq, Functor, Ord)

instance (Display a) => Display (ModuleName a) where
  display = \case
    ModuleName names ->
      "ModuleName: [" <> intercalateMap1 ", " display names <> "]"

data ProperName a
  = ProperName !a !Text
  deriving (Eq, Functor, Ord)

instance (Display a) => Display (ProperName a) where
  display = \case
    ProperName ann name ->
      "ProperName annotation: "
        <> display ann
        <> ", name: "
        <> display name

newtype KindName a
  = KindName (ProperName a)
  deriving (Eq, Functor, Ord)

instance (Display a) => Display (KindName a) where
  display = \case
    KindName name -> "KindName: " <> display name

data Constructors a
  = ConstructorsAnnotation !a !(Constructors a)
  | ConstructorsNone
  | ConstructorsSome (NonEmpty (ProperName a))
  | ConstructorsAll
  deriving (Eq, Functor, Ord)

instance (Display a) => Display (Constructors a) where
  display = \case
    ConstructorsAnnotation ann constructors ->
      "Constructors annotation: "
        <> display ann
        <> ", constructors: "
        <> display constructors
    ConstructorsNone -> "No constructors"
    ConstructorsSome names ->
      "Some constructors: [" <> intercalateMap1 ", " display names <> "]"
    ConstructorsAll -> "All constructors"

data Export a
  = ExportAnnotation !a !(Export a)
  | ExportKind !(KindName a)
  | ExportModule !(ModuleName a)
  | ExportType !(Type a)
  | ExportTypeOperator !(TypeOperator a)
  | ExportValue !Ident
  deriving (Functor)

instance (Display a) => Display (Export a) where
  display = \case
    ExportAnnotation ann export ->
      "Export annotation: "
        <> display ann
        <> ", export: "
        <> display export
    ExportKind name -> "Export kind: " <> display name
    ExportModule name -> "Export module: " <> display name
    ExportType ty -> "Export type: " <> display ty
    ExportTypeOperator op -> "Export type operator: " <> display op
    ExportValue ident -> "Export value: " <> display ident

newtype Ident
  = Ident Text
  deriving (Eq, Ord)

instance Display Ident where
  display = \case
    Ident x -> "Ident: " <> display x

data Type a
  = Type !(ProperName a) !(Constructors a)
  deriving (Eq, Functor, Ord)

instance (Display a) => Display (Type a) where
  display = \case
    Type name constructors ->
      "Type name: "
        <> display name
        <> ", constructors: ("
        <> display constructors
        <> ")"

data TypeOperator a
  = TypeOperator !a !Text
  deriving (Eq, Functor, Ord)

instance (Display a) => Display (TypeOperator a) where
  display = \case
    TypeOperator ann op ->
      "Type Operator annotation: "
        <> display ann
        <> ", op: ("
        <> display op
        <> ")"

data Unannotated
  = Unannotated

instance Display Unannotated where
  display = \case
    Unannotated -> "Unannotated"

data Error
  = EmptyExplicitExports
  | InvalidExport !Language.PureScript.Ident
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

data Sorted
  = Sorted
  deriving (Show)

instance Display Sorted where
  display = \case
    Sorted -> "Sorted"

compareProperName :: ProperName a -> ProperName b -> Ordering
compareProperName x' y' = case (x', y') of
  (ProperName _ x, ProperName _ y) -> compare x y

compareExport :: Export a -> Export b -> Ordering
compareExport x' y' = case (x', y') of
  (ExportAnnotation _annX x, _)    -> compareExport x y'
  (_, ExportAnnotation _annY y)    -> compareExport x' y
  (ExportKind x, ExportKind y)  -> compare (void x) (void y)
  (ExportKind _, ExportModule _) -> GT
  (ExportKind _, ExportType _)  -> LT
  (ExportKind _, ExportTypeOperator _)  -> LT
  (ExportKind _, ExportValue _)  -> LT
  (ExportModule _, ExportKind _)  -> LT
  (ExportModule x, ExportModule y) -> compare (void x) (void y)
  (ExportModule _, ExportType _)  -> LT
  (ExportModule _, ExportTypeOperator _)  -> LT
  (ExportModule _, ExportValue _)  -> LT
  (ExportType _, ExportKind _)  -> GT
  (ExportType _, ExportModule _) -> GT
  (ExportType x, ExportType y) -> compare (void x) (void y)
  (ExportType _, ExportTypeOperator _) -> LT
  (ExportType _, ExportValue _)  -> LT
  (ExportTypeOperator _, ExportKind _)  -> GT
  (ExportTypeOperator _, ExportModule _) -> GT
  (ExportTypeOperator x, ExportTypeOperator y) -> compare (void x) (void y)
  (ExportTypeOperator _, ExportType _) -> LT
  (ExportTypeOperator _, ExportValue _)  -> LT
  (ExportValue _, ExportKind _)  -> GT
  (ExportValue _, ExportModule _)  -> GT
  (ExportValue _, ExportType _)  -> GT
  (ExportValue _, ExportTypeOperator _)  -> GT
  (ExportValue x, ExportValue y)   -> compare x y

fromExport ::
  (IsInvalidExport e, IsMissingName e, IsNotImplemented e, MonadError e f) =>
  Language.PureScript.DeclarationRef ->
  f (Export Unannotated)
fromExport = \case
  Language.PureScript.KindRef _ name -> pure (ExportKind $ fromKindName name)
  Language.PureScript.ModuleRef _ name -> fmap ExportModule (fromModuleName name)
  Language.PureScript.TypeRef _ name constructors ->
    pure (ExportType $ fromType name constructors)
  Language.PureScript.TypeOpRef _ op -> pure (ExportTypeOperator $ fromOpName op)
  Language.PureScript.ValueRef _ ident -> fmap ExportValue (fromIdent ident)
  ref -> throwing _NotImplemented (displayShow ref)

fromExports ::
  ( IsEmptyExplicitExports e
  , IsInvalidExport e
  , IsMissingName e
  , IsNotImplemented e
  , MonadError e f
  ) =>
  [Language.PureScript.DeclarationRef] ->
  f (NonEmpty (Export Unannotated))
fromExports =
  maybe (throwing_ _EmptyExplicitExports) (traverse fromExport) . nonEmpty

fromIdent ::
  (IsInvalidExport e, MonadError e f) =>
  Language.PureScript.Ident ->
  f Ident
fromIdent = \case
  Language.PureScript.Ident ident -> pure (Ident ident)
  ident -> throwing _InvalidExport ident

fromKindName ::
  Language.PureScript.ProperName 'Language.PureScript.KindName ->
  KindName Unannotated
fromKindName = KindName . fromProperName

fromModuleName ::
  (IsMissingName e, MonadError e f) =>
  Language.PureScript.ModuleName ->
  f (ModuleName Unannotated)
fromModuleName = \case
  Language.PureScript.ModuleName names' ->
    maybe (throwing_ _MissingName) pure $ do
      names <- nonEmpty (fmap fromProperName names')
      pure (ModuleName names)

fromOpName ::
  Language.PureScript.OpName 'Language.PureScript.TypeOpName ->
  TypeOperator Unannotated
fromOpName = \case
  Language.PureScript.OpName name -> TypeOperator Unannotated name

fromProperName :: Language.PureScript.ProperName a -> ProperName Unannotated
fromProperName = \case
  Language.PureScript.ProperName name -> ProperName Unannotated name

fromPureScript ::
  (IsError e, IsNotImplemented e, MonadError e f) =>
  Language.PureScript.Module ->
  f (Module Unannotated)
fromPureScript = \case
  Language.PureScript.Module _ _ name' _ exports' -> do
    name <- fromModuleName name'
    exports <- traverse fromExports exports'
    pure (Module Unannotated name exports)

fromType ::
  Language.PureScript.ProperName 'Language.PureScript.TypeName ->
  Maybe [Language.PureScript.ProperName 'Language.PureScript.ConstructorName] ->
  Type Unannotated
fromType name = Type (fromProperName name) . \case
  Nothing -> ConstructorsAll
  Just cs ->
    maybe ConstructorsNone ConstructorsSome (nonEmpty $ fmap fromProperName cs)

sortConstructors :: Constructors a -> Constructors Sorted
sortConstructors = \case
  ConstructorsAnnotation _ann constructors ->
    ConstructorsAnnotation Sorted (sortConstructors constructors)
  ConstructorsNone -> ConstructorsNone
  ConstructorsSome constructors ->
    ConstructorsSome (fmap (Sorted <$) (sortBy compareProperName constructors))
  ConstructorsAll -> ConstructorsAll

sortExports :: Module a -> Module Sorted
sortExports = \case
  Module _ann name exports ->
    Module Sorted (Sorted <$ name) (fmap sortExports' exports)

sortExports' :: NonEmpty (Export a) -> NonEmpty (Export Sorted)
sortExports' = sortBy compareExport . fmap go
  where
  go :: Export a -> Export Sorted
  go = \case
    ExportAnnotation _ann export -> ExportAnnotation Sorted (go export)
    ExportKind x -> ExportKind (Sorted <$ x)
    ExportModule x -> ExportModule (Sorted <$ x)
    ExportType (Type name constructors) ->
      ExportType (Type (Sorted <$ name) (sortConstructors constructors))
    ExportTypeOperator x -> ExportTypeOperator (Sorted <$ x)
    ExportValue x -> ExportValue x
    ExportValueOperator x -> ExportValueOperator (Sorted <$ x)
