module AST where

import "rio" RIO

import "lens" Control.Lens                     (Prism', prism)
import "lens" Control.Monad.Error.Lens         (throwing, throwing_)
import "mtl" Control.Monad.Except              (MonadError)
import "base" Data.List.NonEmpty               (NonEmpty, nonEmpty, sortBy)
import "semigroupoids" Data.Semigroup.Foldable (intercalateMap1)

import qualified "purescript" Language.PureScript

import qualified "this" Annotation
import qualified "this" Name

data Module a
  = Module !a !(Name.Module a) !(Maybe (NonEmpty (Export a)))
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

data Constructors a
  = ConstructorsAnnotation !a !(Constructors a)
  | ConstructorsNone
  | ConstructorsSome (NonEmpty (Name.Proper a))
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
  | ExportClass !(Name.Class a)
  | ExportKind !(Name.Kind a)
  | ExportModule !(Name.Module a)
  | ExportType !(Type a)
  | ExportTypeOperator !(TypeOperator a)
  | ExportValue !Ident
  | ExportValueOperator !(ValueOperator a)
  deriving (Functor)

instance (Display a) => Display (Export a) where
  display = \case
    ExportAnnotation ann export ->
      "Export annotation: "
        <> display ann
        <> ", export: "
        <> display export
    ExportClass name -> "Export class: " <> display name
    ExportKind name -> "Export kind: " <> display name
    ExportModule name -> "Export module: " <> display name
    ExportType ty -> "Export type: " <> display ty
    ExportTypeOperator op -> "Export type operator: " <> display op
    ExportValue ident -> "Export value: " <> display ident
    ExportValueOperator op -> "Export value operator: " <> display op

newtype Ident
  = Ident Text
  deriving (Eq, Ord)

instance Display Ident where
  display = \case
    Ident x -> "Ident: " <> display x

data Type a
  = Type !(Name.Proper a) !(Constructors a)
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

data ValueOperator a
  = ValueOperator !a !Text
  deriving (Eq, Functor, Ord)

instance (Display a) => Display (ValueOperator a) where
  display = \case
    ValueOperator ann op ->
      "Value Operator annotation: "
        <> display ann
        <> ", op: ("
        <> display op
        <> ")"

data Error
  = EmptyExplicitExports
  | InstanceExported !Language.PureScript.Ident
  | InvalidExport !Language.PureScript.Ident
  | ReExportExported !Language.PureScript.ModuleName !Language.PureScript.DeclarationRef

instance Display Error where
  display = \case
    EmptyExplicitExports -> "Module has an empty export list"
    InstanceExported ident ->
      "Module exports a type class instance: "
        <> displayShow ident
        <> ". This is probably a problem in PureScript."
    InvalidExport ident ->
      "Module exports an invalid identifier: " <> displayShow ident
    ReExportExported name ref ->
      "Module exports a re-export explicitly: module name: "
        <> displayShow name
        <> ", declaration ref: "
        <> displayShow ref
        <> ". This is probably a problem in PureScript."

class
  ( IsEmptyExplicitExports error
  , IsInstanceExported error
  , IsInvalidExport error
  , IsReExportExported error
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

class IsInstanceExported error where
  _InstanceExported :: Prism' error Language.PureScript.Ident

instance IsInstanceExported Error where
  _InstanceExported = prism InstanceExported $ \case
    InstanceExported ident -> Right ident
    x -> Left x

class IsInvalidExport error where
  _InvalidExport :: Prism' error Language.PureScript.Ident

instance IsInvalidExport Error where
  _InvalidExport = prism InvalidExport $ \case
    InvalidExport ident -> Right ident
    x -> Left x

class IsReExportExported error where
  _ReExportExported :: Prism' error (Language.PureScript.ModuleName, Language.PureScript.DeclarationRef)

instance IsReExportExported Error where
  _ReExportExported = prism (uncurry ReExportExported) $ \case
    ReExportExported name ref -> Right (name, ref)
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

compareExport :: Export a -> Export b -> Ordering
compareExport x' y' = case (x', y') of
  (ExportAnnotation _annX x, _)                  -> compareExport x y'
  (_, ExportAnnotation _annY y)                  -> compareExport x' y
  (ExportClass x, ExportClass y)                 -> compare (void x) (void y)
  (ExportClass _, ExportKind _)                  -> LT
  (ExportClass _, ExportModule _)                -> GT
  (ExportClass _, ExportType _)                  -> LT
  (ExportClass _, ExportTypeOperator _)          -> LT
  (ExportClass _, ExportValue _)                 -> LT
  (ExportClass _, ExportValueOperator _)         -> LT
  (ExportKind _, ExportClass _)                  -> GT
  (ExportKind x, ExportKind y)                   -> compare (void x) (void y)
  (ExportKind _, ExportModule _)                 -> GT
  (ExportKind _, ExportType _)                   -> LT
  (ExportKind _, ExportTypeOperator _)           -> LT
  (ExportKind _, ExportValue _)                  -> LT
  (ExportKind _, ExportValueOperator _)          -> LT
  (ExportModule _, ExportClass _)                -> LT
  (ExportModule _, ExportKind _)                 -> LT
  (ExportModule x, ExportModule y)               -> compare (void x) (void y)
  (ExportModule _, ExportType _)                 -> LT
  (ExportModule _, ExportTypeOperator _)         -> LT
  (ExportModule _, ExportValue _)                -> LT
  (ExportModule _, ExportValueOperator _)        -> LT
  (ExportType _, ExportClass _)                  -> GT
  (ExportType _, ExportKind _)                   -> GT
  (ExportType _, ExportModule _)                 -> GT
  (ExportType x, ExportType y)                   -> compare (void x) (void y)
  (ExportType _, ExportTypeOperator _)           -> LT
  (ExportType _, ExportValue _)                  -> LT
  (ExportType _, ExportValueOperator _)          -> LT
  (ExportTypeOperator _, ExportClass _)          -> GT
  (ExportTypeOperator _, ExportKind _)           -> GT
  (ExportTypeOperator _, ExportModule _)         -> GT
  (ExportTypeOperator x, ExportTypeOperator y)   -> compare (void x) (void y)
  (ExportTypeOperator _, ExportType _)           -> LT
  (ExportTypeOperator _, ExportValue _)          -> LT
  (ExportTypeOperator _, ExportValueOperator _)  -> LT
  (ExportValue _, ExportClass _)                 -> GT
  (ExportValue _, ExportKind _)                  -> GT
  (ExportValue _, ExportModule _)                -> GT
  (ExportValue _, ExportType _)                  -> GT
  (ExportValue _, ExportTypeOperator _)          -> GT
  (ExportValue x, ExportValue y)                 -> compare x y
  (ExportValue _, ExportValueOperator _)         -> GT
  (ExportValueOperator _, ExportClass _)         -> GT
  (ExportValueOperator _, ExportKind _)          -> GT
  (ExportValueOperator _, ExportModule _)        -> GT
  (ExportValueOperator _, ExportTypeOperator _)  -> GT
  (ExportValueOperator _, ExportType _)          -> GT
  (ExportValueOperator _, ExportValue _)         -> LT
  (ExportValueOperator x, ExportValueOperator y) -> compare (void x) (void y)

fromExport ::
  ( IsInstanceExported e
  , IsInvalidExport e
  , Name.IsMissing e
  , IsReExportExported e
  , MonadError e f
  ) =>
  Language.PureScript.DeclarationRef ->
  f (Export Annotation.Unannotated)
fromExport = \case
  Language.PureScript.KindRef _ name -> pure (ExportKind $ Name.kind name)
  Language.PureScript.ModuleRef _ name -> fmap ExportModule (Name.module' name)
  Language.PureScript.ReExportRef _ name ref ->
    throwing _ReExportExported (name, ref)
  Language.PureScript.TypeRef _ name constructors ->
    pure (ExportType $ fromType name constructors)
  Language.PureScript.TypeClassRef _ name ->
    pure (ExportClass (Name.class' name))
  Language.PureScript.TypeInstanceRef _ name -> throwing _InstanceExported name
  Language.PureScript.TypeOpRef _ op ->
    pure (ExportTypeOperator $ fromTypeOpName op)
  Language.PureScript.ValueRef _ ident -> fmap ExportValue (fromIdent ident)
  Language.PureScript.ValueOpRef _ op ->
    pure (ExportValueOperator $ fromValueOpName op)

fromExports ::
  ( IsEmptyExplicitExports e
  , IsInstanceExported e
  , IsInvalidExport e
  , Name.IsMissing e
  , IsReExportExported e
  , MonadError e f
  ) =>
  [Language.PureScript.DeclarationRef] ->
  f (NonEmpty (Export Annotation.Unannotated))
fromExports =
  maybe (throwing_ _EmptyExplicitExports) (traverse fromExport) . nonEmpty

fromIdent ::
  (IsInvalidExport e, MonadError e f) =>
  Language.PureScript.Ident ->
  f Ident
fromIdent = \case
  Language.PureScript.Ident ident -> pure (Ident ident)
  ident -> throwing _InvalidExport ident

fromTypeOpName ::
  Language.PureScript.OpName 'Language.PureScript.TypeOpName ->
  TypeOperator Annotation.Unannotated
fromTypeOpName = \case
  Language.PureScript.OpName name -> TypeOperator Annotation.Unannotated name

fromValueOpName ::
  Language.PureScript.OpName 'Language.PureScript.ValueOpName ->
  ValueOperator Annotation.Unannotated
fromValueOpName = \case
  Language.PureScript.OpName name -> ValueOperator Annotation.Unannotated name

fromPureScript ::
  (IsError e, Name.IsMissing e, MonadError e f) =>
  Language.PureScript.Module ->
  f (Module Annotation.Unannotated)
fromPureScript = \case
  Language.PureScript.Module _ _ name' _ exports' -> do
    name <- Name.module' name'
    exports <- traverse fromExports exports'
    pure (Module Annotation.Unannotated name exports)

fromType ::
  Language.PureScript.ProperName 'Language.PureScript.TypeName ->
  Maybe [Language.PureScript.ProperName 'Language.PureScript.ConstructorName] ->
  Type Annotation.Unannotated
fromType name = Type (Name.proper name) . \case
  Nothing -> ConstructorsAll
  Just cs ->
    maybe ConstructorsNone ConstructorsSome (nonEmpty $ fmap Name.proper cs)

sortConstructors :: Constructors a -> Constructors Annotation.Sorted
sortConstructors = \case
  ConstructorsAnnotation _ann constructors ->
    ConstructorsAnnotation Annotation.Sorted (sortConstructors constructors)
  ConstructorsNone -> ConstructorsNone
  ConstructorsSome constructors ->
    ConstructorsSome (fmap (Annotation.Sorted <$) (sortBy Name.compareProper constructors))
  ConstructorsAll -> ConstructorsAll

sortExports :: Module a -> Module Annotation.Sorted
sortExports = \case
  Module _ann name exports ->
    Module Annotation.Sorted (Annotation.Sorted <$ name) (fmap sortExports' exports)

sortExports' :: NonEmpty (Export a) -> NonEmpty (Export Annotation.Sorted)
sortExports' = sortBy compareExport . fmap go
  where
  go :: Export a -> Export Annotation.Sorted
  go = \case
    ExportAnnotation _ann export ->
      ExportAnnotation Annotation.Sorted (go export)
    ExportClass x -> ExportClass (Annotation.Sorted <$ x)
    ExportKind x -> ExportKind (Annotation.Sorted <$ x)
    ExportModule x -> ExportModule (Annotation.Sorted <$ x)
    ExportType (Type name constructors) ->
      ExportType (Type (Annotation.Sorted <$ name) (sortConstructors constructors))
    ExportTypeOperator x -> ExportTypeOperator (Annotation.Sorted <$ x)
    ExportValue x -> ExportValue x
    ExportValueOperator x -> ExportValueOperator (Annotation.Sorted <$ x)
