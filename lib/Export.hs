module Export where

import "rio" RIO

import "freer-simple" Control.Monad.Freer        (Eff, Members)
import "freer-simple" Control.Monad.Freer.Error  (Error, throwError)
import "base" Data.List                          (intersperse)
import "base" Data.List.NonEmpty
    ( NonEmpty((:|))
    , nonEmpty
    , sortBy
    )
import "prettyprinter" Data.Text.Prettyprint.Doc
    ( Doc
    , align
    , comma
    , dot
    , flatAlt
    , group
    , indent
    , line
    , parens
    , pretty
    , space
    , (<+>)
    )

import qualified "purescript" Language.PureScript

import "this" Variations (Variations(Variations, multiLine, singleLine))

import qualified "this" Annotation
import qualified "this" Log
import qualified "this" Name
import qualified "this" Variations

data Constructors a
  = ConstructorsAnnotation !a !(Constructors a)
  | ConstructorsNone
  | ConstructorsSome !(NonEmpty (Name.Proper a))
  | ConstructorsAll
  deriving (Eq, Functor, Ord, Show)

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
  | ExportValueOperator !(Name.ValueOperator a)
  deriving (Functor, Show)

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
  ExportValueOperator op -> pure (Name.docFromValueOperator op)

export ::
  ( Members
    '[ Error InstanceExported
     , Error InvalidExport
     , Error Name.Missing
     , Error ReExportExported
     ]
    e
  ) =>
  Language.PureScript.DeclarationRef ->
  Eff e (Export Annotation.Unannotated)
export = \case
  Language.PureScript.KindRef _ name -> pure (ExportKind $ Name.kind name)
  Language.PureScript.ModuleRef _ name -> fmap ExportModule (Name.module' name)
  Language.PureScript.ReExportRef _ name ref ->
    throwError (ReExportExported name ref)
  Language.PureScript.TypeRef _ name constructors ->
    pure (ExportType $ type' name constructors)
  Language.PureScript.TypeClassRef _ name ->
    pure (ExportClass (Name.class' name))
  Language.PureScript.TypeInstanceRef _ name -> throwError (InstanceExported name)
  Language.PureScript.TypeOpRef _ op ->
    pure (ExportTypeOperator $ typeOperator op)
  Language.PureScript.ValueRef _ ident -> fmap ExportValue (value ident)
  Language.PureScript.ValueOpRef _ op ->
    pure (ExportValueOperator $ Name.valueOperator op)

newtype Exports a
  = Exports (Maybe (NonEmpty (Export a)))
  deriving (Show)

instance (Log.Inspect a) => Log.Inspect (Exports a)

data Sorted
  = NoExports
  | Sorted
      !(Export Annotation.Sorted)
      ![Name.Module Annotation.Sorted]
      ![Name.Class Annotation.Sorted]
      ![Name.Kind Annotation.Sorted]
      ![TypeOperator Annotation.Sorted]
      ![Type Annotation.Sorted]
      ![Name.ValueOperator Annotation.Sorted]
      ![Value Annotation.Sorted]
  deriving (Show)

instance Log.Inspect Sorted

dynamic, static :: Sorted -> Doc b
(dynamic, static) = (dynamic', static')
  where
  dynamic' x = case x of
    NoExports -> mempty
    Sorted export' ms cs ks tOs ts vOs vs ->
      group (flatAlt (static' x) (space <> parens doc))
        where
        doc =
          foldMap
            Variations.singleLine
            (intersperse (pure (comma <> space)) exports)
        exports = exportsDoc (insertExport ms cs ks tOs ts vOs vs export')
  static' = \case
    NoExports -> mempty
    Sorted export' ms cs ks tOs ts vOs vs ->
      line <> indent 2 (align (parens (space <> doc <> line)))
        where
        doc =
          foldMap
            Variations.multiLine
            (intersperse (pure (line <> comma <> space)) exports)
        exports = exportsDoc (insertExport ms cs ks tOs ts vOs vs export')
  exportsDoc (ms, cs, ks, tOs, ts, vOs, vs) =
    fmap (docFromExport . ExportModule) ms
      <> fmap (docFromExport . ExportClass) cs
      <> fmap (docFromExport . ExportKind) ks
      <> fmap (docFromExport . ExportTypeOperator) tOs
      <> fmap (docFromExport . ExportType) ts
      <> fmap (docFromExport . ExportValueOperator) vOs
      <> fmap (docFromExport . ExportValue) vs
  insertExport ms cs ks tOs ts vOs vs = \case
    ExportAnnotation _ann x -> insertExport ms cs ks tOs ts vOs vs x
    ExportModule x -> (x : ms, cs, ks, tOs, ts, vOs, vs)
    ExportClass x -> (ms, x : cs, ks, tOs, ts, vOs, vs)
    ExportKind x -> (ms, cs, x : ks, tOs, ts, vOs, vs)
    ExportTypeOperator x -> (ms, cs, ks, x : tOs, ts, vOs, vs)
    ExportType x -> (ms, cs, ks, tOs, x : ts, vOs, vs)
    ExportValueOperator x -> (ms, cs, ks, tOs, ts, x : vOs, vs)
    ExportValue x -> (ms, cs, ks, tOs, ts, vOs, x : vs)

data Type a
  = Type !(Name.Proper a) !(Constructors a)
  deriving (Eq, Functor, Ord, Show)

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
  deriving (Eq, Functor, Ord, Show)

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
  deriving (Eq, Functor, Ord, Show)

docFromValue :: Value a -> Doc b
docFromValue = \case
  Value _ann name -> pretty name

value ::
  (Members '[Error InvalidExport] e) =>
  Language.PureScript.Ident ->
  Eff e (Value Annotation.Unannotated)
value = \case
  Language.PureScript.Ident ident -> pure (Value Annotation.Unannotated ident)
  ident -> throwError (InvalidExport ident)

fromPureScript ::
  ( Members
    '[ Error EmptyExplicitExports
     , Error InstanceExported
     , Error InvalidExport
     , Error Name.Missing
     , Error ReExportExported
     ]
    e
  ) =>
  [Language.PureScript.DeclarationRef] ->
  Eff e (NonEmpty (Export Annotation.Unannotated))
fromPureScript =
  maybe (throwError EmptyExplicitExports) (traverse export) . nonEmpty

sortConstructors :: Constructors a -> Constructors Annotation.Sorted
sortConstructors = \case
  ConstructorsAnnotation _ann constructors ->
    ConstructorsAnnotation Annotation.Sorted (sortConstructors constructors)
  ConstructorsNone -> ConstructorsNone
  ConstructorsSome constructors ->
    ConstructorsSome (fmap (Annotation.Sorted <$) (sortBy Name.compareProper constructors))
  ConstructorsAll -> ConstructorsAll

sort :: Exports a -> Sorted
sort = \case
  Exports Nothing -> NoExports
  Exports (Just exports) ->
    Sorted headExport ms cs ks tOs ts vOs vs
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
    headExport :| tailExports = sortBy compareExport (fmap go exports)
    (ms, cs, ks, tOs, ts, vOs, vs) =
      foldl' partition ([], [], [], [], [], [], []) (reverse tailExports)
    partition acc@(ms', cs', ks', tOs', ts', vOs', vs') = \case
      ExportAnnotation _ann x -> partition acc x
      ExportModule x -> (x : ms', cs', ks', tOs', ts', vOs', vs')
      ExportClass x -> (ms', x : cs', ks', tOs', ts', vOs', vs')
      ExportKind x -> (ms', cs', x : ks', tOs', ts', vOs', vs')
      ExportTypeOperator x -> (ms', cs', ks', x : tOs', ts', vOs', vs')
      ExportType x -> (ms', cs', ks', tOs', x : ts', vOs', vs')
      ExportValueOperator x -> (ms', cs', ks', tOs', ts', x : vOs', vs')
      ExportValue x -> (ms', cs', ks', tOs', ts', vOs', x : vs')

-- Errors

type Errors
  = '[ Error EmptyExplicitExports
     , Error InstanceExported
     , Error InvalidExport
     , Error ReExportExported
     ]

data EmptyExplicitExports
  = EmptyExplicitExports

newtype InstanceExported
  = InstanceExported Language.PureScript.Ident

newtype InvalidExport
  = InvalidExport Language.PureScript.Ident

data ReExportExported
  = ReExportExported !Language.PureScript.ModuleName !Language.PureScript.DeclarationRef

instance Display EmptyExplicitExports where
  display = \case
    EmptyExplicitExports -> "Module has an empty export list"

instance Display InstanceExported where
  display = \case
    InstanceExported ident ->
      "Module exports a type class instance: "
        <> displayShow ident
        <> ". This is probably a problem in PureScript."

instance Display InvalidExport where
  display = \case
    InvalidExport ident ->
      "Module exports an invalid identifier: " <> displayShow ident

instance Display ReExportExported where
  display = \case
    ReExportExported name ref ->
      "Module exports a re-export explicitly: module name: "
        <> displayShow name
        <> ", declaration ref: "
        <> displayShow ref
        <> ". This is probably a problem in PureScript."
