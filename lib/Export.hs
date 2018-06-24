module Export where

import "rio" RIO

import "lens" Control.Lens                       (Prism', prism)
import "lens" Control.Monad.Error.Lens           (throwing, throwing_)
import "mtl" Control.Monad.Except                (MonadError)
import "base" Data.List.NonEmpty                 (NonEmpty, nonEmpty, sortBy)
import "semigroupoids" Data.Semigroup.Foldable   (intercalateMap1)
import "prettyprinter" Data.Text.Prettyprint.Doc
    ( Doc
    , dot
    , indent
    , line
    , parens
    , pretty
    , (<+>)
    )

import qualified "purescript" Language.PureScript

import "this" Variations (Variations(Variations, multiLine, singleLine))

import qualified "this" Annotation
import qualified "this" Name
import qualified "this" Variations

data Constructors a
  = ConstructorsAnnotation !a !(Constructors a)
  | ConstructorsNone
  | ConstructorsSome !(NonEmpty (Name.Proper a))
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

docFromConstructors :: Constructors Annotation.Sorted -> Variations (Doc a)
docFromConstructors = \case
  ConstructorsAnnotation _ann constructors -> docFromConstructors constructors
  ConstructorsNone -> pure mempty
  ConstructorsSome constructors' ->
    Variations
      { multiLine = line <> indent 4 (multiLine constructors)
      , singleLine = singleLine constructors
      }
    where
    constructors =
      Variations.parenthesize (pure . Name.docFromProper) constructors'
  ConstructorsAll -> pure (parens (dot <> dot))

data Export a
  = ExportAnnotation !a !(Export a)
  | ExportClass !(Name.Class a)
  | ExportKind !(Name.Kind a)
  | ExportModule !(Name.Module a)
  | ExportType !(Type a)
  | ExportTypeOperator !(TypeOperator a)
  | ExportValue !(Value a)
  | ExportValueOperator !(ValueOperator a)
  deriving (Functor)

instance (Display a) => Display (Export a) where
  display = \case
    ExportAnnotation ann x ->
      "Export annotation: "
        <> display ann
        <> ", export: "
        <> display x
    ExportClass name -> "Export class: " <> display name
    ExportKind name -> "Export kind: " <> display name
    ExportModule name -> "Export module: " <> display name
    ExportType ty -> "Export type: " <> display ty
    ExportTypeOperator op -> "Export type operator: " <> display op
    ExportValue ident -> "Export value: " <> display ident
    ExportValueOperator op -> "Export value operator: " <> display op

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
  (ExportValue x, ExportValue y)                 -> compare (void x) (void y)
  (ExportValue _, ExportValueOperator _)         -> GT
  (ExportValueOperator _, ExportClass _)         -> GT
  (ExportValueOperator _, ExportKind _)          -> GT
  (ExportValueOperator _, ExportModule _)        -> GT
  (ExportValueOperator _, ExportTypeOperator _)  -> GT
  (ExportValueOperator _, ExportType _)          -> GT
  (ExportValueOperator _, ExportValue _)         -> LT
  (ExportValueOperator x, ExportValueOperator y) -> compare (void x) (void y)

docFromExport :: Export Annotation.Sorted -> Variations (Doc a)
docFromExport = \case
  ExportAnnotation _ann export' -> docFromExport export'
  ExportClass name -> pure ("class" <+> Name.docFromClass name)
  ExportKind name -> pure ("kind" <+> Name.docFromKind name)
  ExportModule name -> pure ("module" <+> Name.docFromModule name)
  ExportType ty -> docFromType ty
  ExportTypeOperator op -> pure ("type" <+> Export.docFromTypeOperator op)
  ExportValue value' -> pure (Export.docFromValue value')
  ExportValueOperator op -> pure (Export.docFromValueOperator op)

export ::
  ( IsInstanceExported e
  , IsInvalidExport e
  , Name.IsMissing e
  , IsReExportExported e
  , MonadError e f
  ) =>
  Language.PureScript.DeclarationRef ->
  f (Export Annotation.Unannotated)
export = \case
  Language.PureScript.KindRef _ name -> pure (ExportKind $ Name.kind name)
  Language.PureScript.ModuleRef _ name -> fmap ExportModule (Name.module' name)
  Language.PureScript.ReExportRef _ name ref ->
    throwing _ReExportExported (name, ref)
  Language.PureScript.TypeRef _ name constructors ->
    pure (ExportType $ type' name constructors)
  Language.PureScript.TypeClassRef _ name ->
    pure (ExportClass (Name.class' name))
  Language.PureScript.TypeInstanceRef _ name -> throwing _InstanceExported name
  Language.PureScript.TypeOpRef _ op ->
    pure (ExportTypeOperator $ typeOperator op)
  Language.PureScript.ValueRef _ ident -> fmap ExportValue (value ident)
  Language.PureScript.ValueOpRef _ op ->
    pure (ExportValueOperator $ valueOperator op)

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

docFromType :: Type Annotation.Sorted -> Variations (Doc b)
docFromType = \case
  Type name constructors ->
    fmap (Name.docFromProper name <>) (docFromConstructors constructors)

type' ::
  Language.PureScript.ProperName 'Language.PureScript.TypeName ->
  Maybe [Language.PureScript.ProperName 'Language.PureScript.ConstructorName] ->
  Type Annotation.Unannotated
type' name = Type (Name.proper name) . \case
  Nothing -> ConstructorsAll
  Just cs ->
    maybe ConstructorsNone ConstructorsSome (nonEmpty $ fmap Name.proper cs)

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

docFromTypeOperator :: TypeOperator a -> Doc b
docFromTypeOperator = \case
  TypeOperator _ann op -> parens (pretty op)

typeOperator ::
  Language.PureScript.OpName 'Language.PureScript.TypeOpName ->
  TypeOperator Annotation.Unannotated
typeOperator = \case
  Language.PureScript.OpName name -> TypeOperator Annotation.Unannotated name

data Value a
  = Value !a !Text
  deriving (Eq, Functor, Ord)

instance (Display a) => Display (Value a) where
  display = \case
    Value ann x ->
      "Value annotation:"
      <> display ann
      <> ", value: "
      <> display x

docFromValue :: Value a -> Doc b
docFromValue = \case
  Value _ann name -> pretty name

value ::
  (IsInvalidExport e, MonadError e f) =>
  Language.PureScript.Ident ->
  f (Value Annotation.Unannotated)
value = \case
  Language.PureScript.Ident ident -> pure (Value Annotation.Unannotated ident)
  ident -> throwing _InvalidExport ident

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

docFromValueOperator :: ValueOperator a -> Doc b
docFromValueOperator = \case
  ValueOperator _ann op -> parens (pretty op)

valueOperator ::
  Language.PureScript.OpName 'Language.PureScript.ValueOpName ->
  ValueOperator Annotation.Unannotated
valueOperator = \case
  Language.PureScript.OpName name -> ValueOperator Annotation.Unannotated name

fromPureScript ::
  ( IsEmptyExplicitExports e
  , IsInstanceExported e
  , IsInvalidExport e
  , Name.IsMissing e
  , IsReExportExported e
  , MonadError e f
  ) =>
  [Language.PureScript.DeclarationRef] ->
  f (NonEmpty (Export Annotation.Unannotated))
fromPureScript =
  maybe (throwing_ _EmptyExplicitExports) (traverse export) . nonEmpty

sortConstructors :: Constructors a -> Constructors Annotation.Sorted
sortConstructors = \case
  ConstructorsAnnotation _ann constructors ->
    ConstructorsAnnotation Annotation.Sorted (sortConstructors constructors)
  ConstructorsNone -> ConstructorsNone
  ConstructorsSome constructors ->
    ConstructorsSome (fmap (Annotation.Sorted <$) (sortBy Name.compareProper constructors))
  ConstructorsAll -> ConstructorsAll

sort :: NonEmpty (Export a) -> NonEmpty (Export Annotation.Sorted)
sort = sortBy compareExport . fmap go
  where
  go :: Export a -> Export Annotation.Sorted
  go = \case
    ExportAnnotation _ann x -> ExportAnnotation Annotation.Sorted (go x)
    ExportClass x -> ExportClass (Annotation.Sorted <$ x)
    ExportKind x -> ExportKind (Annotation.Sorted <$ x)
    ExportModule x -> ExportModule (Annotation.Sorted <$ x)
    ExportType (Type name constructors) ->
      ExportType (Type (Annotation.Sorted <$ name) (sortConstructors constructors))
    ExportTypeOperator x -> ExportTypeOperator (Annotation.Sorted <$ x)
    ExportValue x -> ExportValue (Annotation.Sorted <$ x)
    ExportValueOperator x -> ExportValueOperator (Annotation.Sorted <$ x)

-- Errors

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
