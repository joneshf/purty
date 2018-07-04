module Kind where

import "rio" RIO

import "freer-simple" Control.Monad.Freer        (Eff, Members)
import "freer-simple" Control.Monad.Freer.Error  (throwError, Error)
import "prettyprinter" Data.Text.Prettyprint.Doc ((<+>), parens, braces, Doc)

import qualified "purescript" Language.PureScript

import qualified "this" Annotation
import qualified "this" Name

data Kind a
  = KindAnnotation !a !(Kind a)
  | KindFunction !(Kind a) !(Kind a)
  | KindName !(Name.Qualified Name.Kind a)
  | KindRow !(Kind a)
  deriving (Functor)

instance (Display a) => Display (Kind a) where
  display = \case
    KindAnnotation ann x ->
      "Kind annotation: "
        <> display ann
        <> ", kind: "
        <> display x
    KindFunction x y ->
      "Kind Function: "
        <> display x
        <> " -> "
        <> display y
    KindName x ->
      "Kind Name: "
        <> display x
    KindRow x ->
      "Kind Row: "
        <> display x

doc :: Kind Annotation.Normalized -> Doc b
doc = \case
  KindAnnotation Annotation.None x -> doc x
  KindAnnotation Annotation.Braces x -> braces (doc x)
  KindAnnotation Annotation.Parens x -> parens (doc x)
  KindFunction x y -> doc x <+> "->" <+> doc y
  KindName x -> Name.docFromQualified Name.docFromKind x
  KindRow x -> "#" <+> doc x

fromPureScript ::
  (Members '[Error InferredKind, Error Name.Missing] e) =>
  Language.PureScript.Kind ->
  Eff e (Kind Annotation.Unannotated)
fromPureScript = \case
  Language.PureScript.FunKind x y ->
    KindFunction <$> fromPureScript x <*> fromPureScript y
  Language.PureScript.KUnknown _ -> throwError InferredKind
  Language.PureScript.NamedKind x ->
    fmap KindName (Name.qualified (pure . Name.kind) x)
  Language.PureScript.Row x -> fmap KindRow (fromPureScript x)

normalize :: Kind a -> Kind Annotation.Normalized
normalize = \case
  KindAnnotation _ann x -> normalize x
  KindFunction x@KindFunction {} y ->
    KindFunction (KindAnnotation Annotation.Parens $ normalize x) (normalize y)
  KindFunction x y -> KindFunction (normalize x) (normalize y)
  KindName x -> KindName (Annotation.None <$ x)
  KindRow x@KindFunction {} ->
    KindRow (KindAnnotation Annotation.Parens $ normalize x)
  KindRow x -> KindRow (normalize x)

-- Errors

type Errors
  = '[ Error InferredKind
     ]

data InferredKind
  = InferredKind

instance Display InferredKind where
  display = \case
    InferredKind ->
      "The compiler inferred a kind."
        <> " But, only kinds in the source file should exist at this point."
        <> " We are either using the wrong function from the PureScript library,"
        <> " or there's a problem in the PureScript library."
