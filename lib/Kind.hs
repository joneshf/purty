module Kind where

import "rio" RIO

import "freer-simple" Control.Monad.Freer        (Eff, Members)
import "freer-simple" Control.Monad.Freer.Error  (Error, throwError)
import "prettyprinter" Data.Text.Prettyprint.Doc
    ( Doc
    , braces
    , indent
    , line
    , parens
    , (<+>)
    )

import qualified "purescript" Language.PureScript

import qualified "this" Annotation
import qualified "this" Name
import qualified "this" Variations

data Kind a
  = KindAnnotation !a !(Kind a)
  | KindFunction !(Kind a) !(Kind a)
  | KindName !(Name.Qualified Name.Kind a)
  | KindRow !(Kind a)
  deriving (Functor, Show)

doc :: Kind Annotation.Normalized -> Variations.Variations (Doc b)
doc = \case
  KindAnnotation Annotation.None x -> doc x
  KindAnnotation Annotation.Braces x -> fmap braces (doc x)
  KindAnnotation Annotation.Parens x -> fmap parens (doc x)
  KindFunction x y ->
    Variations.Variations { Variations.multiLine, Variations.singleLine }
      where
      multiLine =
        Variations.multiLine (doc x) <+> "->"
          <> line
          <> indent 2 (Variations.multiLine $ doc y)
      singleLine =
        Variations.singleLine (doc x) <+> "->" <+> Variations.singleLine (doc y)
  KindName x -> pure (Name.docFromQualified Name.docFromKind x)
  KindRow x -> fmap ("#" <+>) (doc x)

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
