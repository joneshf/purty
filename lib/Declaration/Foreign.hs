module Declaration.Foreign where

import "rio" RIO hiding (Data)

import "freer-simple" Control.Monad.Freer        (Eff, Members)
import "freer-simple" Control.Monad.Freer.Error  (Error)
import "prettyprinter" Data.Text.Prettyprint.Doc
    ( Doc
    , colon
    , indent
    , line
    , (<+>)
    )

import qualified "purescript" Language.PureScript

import qualified "this" Annotation
import qualified "this" Comment
import qualified "this" Kind
import qualified "this" Name
import qualified "this" Type
import qualified "this" Variations

data Data a
  = Data !Comment.Comments !(Name.Type a) !(Kind.Kind a)
  deriving (Functor, Show)

data' ::
  ( Members
    '[ Error Kind.InferredKind
     , Error Name.Missing
     ]
    e
  ) =>
  Language.PureScript.SourceAnn ->
  Language.PureScript.ProperName 'Language.PureScript.TypeName ->
  Language.PureScript.Kind ->
  Eff e (Data Annotation.Unannotated)
data' (_, comments') type'' kind'' = do
  let comments = Comment.comments comments'
      type' = Name.type' type''
  kind' <- Kind.fromPureScript kind''
  pure (Data comments type' kind')

docFromData :: Data Annotation.Normalized -> Variations.Variations (Doc a)
docFromData = \case
  Data comments name kind' ->
    Variations.Variations { Variations.multiLine, Variations.singleLine }
      where
      multiLine =
        Comment.docFromComments comments
          <> "foreign import data" <+> Name.docFromType name
          <+> colon <> colon
          <> line
          <> indent 2 (Variations.multiLine $ Kind.doc kind')
          <> line
      singleLine =
        Comment.docFromComments comments
          <> "foreign import data" <+> Name.docFromType name
          <+> colon <> colon
          <+> Variations.singleLine (Kind.doc kind')
          <> line

normalizeData :: Data a -> Data Annotation.Normalized
normalizeData = \case
  Data comments type' kind' ->
    Data comments (Annotation.None <$ type') (Kind.normalize kind')

data Kind a
  = Kind !Comment.Comments (Name.Kind a)
  deriving (Functor, Show)

docFromKind :: Kind Annotation.Normalized -> Doc a
docFromKind = \case
  Kind comments name ->
    Comment.docFromComments comments
      <> "foreign import kind" <+> Name.docFromKind name
      <> line

kind ::
  Language.PureScript.SourceAnn ->
  Language.PureScript.ProperName 'Language.PureScript.KindName ->
  Kind Annotation.Unannotated
kind (_, comments') = Kind (Comment.comments comments'). Name.kind


normalizeKind :: Kind a -> Kind Annotation.Normalized
normalizeKind = \case
  Kind comments name -> Kind comments (Annotation.None <$ name)

data Value a
  = Value !Comment.Comments !(Name.Common a) !(Type.Type a)
  deriving (Functor, Show)

docFromValue :: Value Annotation.Normalized -> Variations.Variations (Doc a)
docFromValue = \case
  Value comments name type' ->
    Variations.Variations { Variations.multiLine, Variations.singleLine }
      where
      multiLine =
        Comment.docFromComments comments
          <> "foreign import" <+> Name.docFromCommon name
          <+> colon <> colon
          <> line
          <> indent 2 (Variations.multiLine $ Type.doc type')
          <> line
      singleLine =
        Comment.docFromComments comments
          <> "foreign import" <+> Name.docFromCommon name
          <+> colon <> colon <+> Variations.singleLine (Type.doc type')
          <> line

normalizeValue :: Value a -> Value Annotation.Normalized
normalizeValue = \case
  Value comments name type' ->
    Value comments (Annotation.None <$ name) (Type.normalize type')

value ::
  ( Members
    '[ Error Kind.InferredKind
     , Error Name.InvalidCommon
     , Error Name.Missing
     , Error Type.InferredConstraintData
     , Error Type.InferredForallWithSkolem
     , Error Type.InferredSkolem
     , Error Type.InferredType
     , Error Type.InfixTypeNotTypeOp
     , Error Type.PrettyPrintForAll
     , Error Type.PrettyPrintFunction
     , Error Type.PrettyPrintObject
     ]
    e
  ) =>
  Language.PureScript.SourceAnn ->
  Language.PureScript.Ident ->
  Language.PureScript.Type ->
  Eff e (Value Annotation.Unannotated)
value (_, comments') name' type'' = do
  let comments = Comment.comments comments'
  name <- Name.common name'
  type' <- Type.fromPureScript type''
  pure (Value comments name type')
