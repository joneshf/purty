module Declaration.DataType where

import "rio" RIO hiding (Data)

import "freer-simple" Control.Monad.Freer        (Eff, Member)
import "freer-simple" Control.Monad.Freer.Error  (Error, throwError)
import "base" Data.List                          (intersperse)
import "semigroupoids" Data.Semigroup.Foldable   (intercalateMap1)
import "prettyprinter" Data.Text.Prettyprint.Doc
    ( Doc
    , align
    , braces
    , equals
    , indent
    , line
    , parens
    , pipe
    , space
    , (<+>)
    )
import "base" GHC.Exts                           (IsList(fromList))

import qualified "purescript" Language.PureScript

import qualified "this" Annotation
import qualified "this" Comment
import qualified "this" Kind
import qualified "this" List
import qualified "this" Name
import qualified "this" Type
import qualified "this" Variations

data Alternate a
  = Alternate !a !(Name.Constructor a) !(List.List (Type.Type a))
  deriving (Functor, Show)

alternate ::
  ( Member (Error Kind.InferredKind) e
  , Member (Error Name.Missing) e
  , Member (Error Type.InferredConstraintData) e
  , Member (Error Type.InferredForallWithSkolem) e
  , Member (Error Type.InferredSkolem) e
  , Member (Error Type.InferredType) e
  , Member (Error Type.InfixTypeNotTypeOp) e
  , Member (Error Type.PrettyPrintForAll) e
  , Member (Error Type.PrettyPrintFunction) e
  , Member (Error Type.PrettyPrintObject) e
  ) =>
  ( Language.PureScript.ProperName 'Language.PureScript.ConstructorName
  , [Language.PureScript.Type]
  ) ->
  Eff e (Alternate Annotation.Unannotated)
alternate = \case
  (x', y') -> do
    let x = Name.constructor x'
    y <- fromList <$> traverse Type.fromPureScript y'
    pure (Alternate Annotation.Unannotated x y)

docFromAlternate :: Alternate Annotation.Normalized -> Doc a
docFromAlternate = \case
  Alternate ann x y -> annotate doc
    where
    annotate = case ann of
      Annotation.None   -> id
      Annotation.Braces -> braces
      Annotation.Parens -> parens
    doc = Name.docFromConstructor x <> List.list' go y
    go types =
      space <> intercalateMap1 space (Variations.singleLine . Type.doc) types

normalizeAlternate :: Alternate a -> Alternate Annotation.Normalized
normalizeAlternate = \case
  Alternate _ann x y ->
    Alternate Annotation.None (Annotation.None <$ x) (fmap Type.normalize y)

data Data a
  = Data
      !Comment.Comments
      !(Name.Proper a)
      !(Type.Variables a)
      !(List.List (Alternate a))
  deriving (Functor, Show)

data' ::
  ( Member (Error Kind.InferredKind) e
  , Member (Error Name.Missing) e
  , Member (Error Type.InferredConstraintData) e
  , Member (Error Type.InferredForallWithSkolem) e
  , Member (Error Type.InferredSkolem) e
  , Member (Error Type.InferredType) e
  , Member (Error Type.InfixTypeNotTypeOp) e
  , Member (Error Type.PrettyPrintForAll) e
  , Member (Error Type.PrettyPrintFunction) e
  , Member (Error Type.PrettyPrintObject) e
  ) =>
  Language.PureScript.SourceAnn ->
  Language.PureScript.ProperName 'Language.PureScript.TypeName ->
  [(Text, Maybe Language.PureScript.Kind)] ->
  [ ( Language.PureScript.ProperName 'Language.PureScript.ConstructorName
    , [ Language.PureScript.Type
      ]
    )
  ] ->
  Eff e (Data Annotation.Unannotated)
data' (_, comments') name variables' constructors = do
  let comments = Comment.comments comments'
  alternates <- fromList <$> traverse alternate constructors
  variables <- Type.variables variables'
  pure (Data comments (Name.proper name) variables alternates)

docFromData :: Data Annotation.Normalized -> Doc a
docFromData = \case
  Data comments name variables (List.NonEmpty alternates) ->
    Comment.docFromComments comments
      <> "data" <+> Name.docFromProper name <> Type.docFromVariables variables
      <> line <> indent 2 (align doc)
      <> line
      where
      doc =
        equals
          <+> intercalateMap1 (line <> pipe <> space) docFromAlternate alternates
  Data comments name variables List.Empty ->
    Comment.docFromComments comments
      <> "data" <+> Name.docFromProper name <> Type.docFromVariables variables
      <> line

normalizeData :: Data a -> Data Annotation.Normalized
normalizeData = \case
  Data comments name typeVariables alternates ->
    Data
      comments
      (Annotation.None <$ name)
      (Type.normalizeVariables typeVariables)
      (fmap normalizeAlternate alternates)

data Newtype a
  = Newtype
    !Comment.Comments
    !(Name.Proper a)
    !(Type.Variables a)
    !(Name.Constructor a)
    !(Type.Type a)
  deriving (Functor, Show)

docFromNewtype :: Newtype Annotation.Normalized -> Doc a
docFromNewtype = \case
  Newtype comments name variables constructor type'' ->
    Comment.docFromComments comments
      <> "newtype" <+> Name.docFromProper name <> Type.docFromVariables variables
      <> line <> indent 2 (equals <+> docConstructor <+> docType)
      <> line
      where
      docConstructor = Name.docFromConstructor constructor
      docType = Variations.singleLine (Type.doc type'')

newtype' ::
  ( Member (Error WrongNewtypeConstructors) e
  , Member (Error Kind.InferredKind) e
  , Member (Error Name.Missing) e
  , Member (Error Type.InferredConstraintData) e
  , Member (Error Type.InferredForallWithSkolem) e
  , Member (Error Type.InferredSkolem) e
  , Member (Error Type.InferredType) e
  , Member (Error Type.InfixTypeNotTypeOp) e
  , Member (Error Type.PrettyPrintForAll) e
  , Member (Error Type.PrettyPrintFunction) e
  , Member (Error Type.PrettyPrintObject) e
  ) =>
  Language.PureScript.SourceAnn ->
  Language.PureScript.ProperName 'Language.PureScript.TypeName ->
  [(Text, Maybe Language.PureScript.Kind)] ->
  [ ( Language.PureScript.ProperName 'Language.PureScript.ConstructorName
    , [ Language.PureScript.Type
      ]
    )
  ] ->
  Eff e (Newtype Annotation.Unannotated)
newtype' (_, comments') name' variables' = \case
  [(constructor', [ty'])] -> do
    let comments = Comment.comments comments'
    ty <- Type.fromPureScript ty'
    variables <- Type.variables variables'
    let constructor = Name.constructor constructor'
        name = Name.proper name'
    pure (Newtype comments name variables constructor ty)
  constructors ->
    throwError (WrongNewtypeConstructors name' constructors)

normalizeNewtype :: Newtype a -> Newtype Annotation.Normalized
normalizeNewtype = \case
  Newtype comments name variables constructor type'' ->
    Newtype
      comments
      (Annotation.None <$ name)
      (Type.normalizeVariables variables)
      (Annotation.None <$ constructor)
      (Type.normalize type'')

displayList :: Display a => [a] -> Utf8Builder
displayList xs = "[" <> fold (intersperse ", " (display <$> xs)) <> "]"

-- Errors

type Errors
  = '[  Error WrongNewtypeConstructors
     ]

data WrongNewtypeConstructors
  = WrongNewtypeConstructors
      !(Language.PureScript.ProperName 'Language.PureScript.TypeName)
      ![ ( Language.PureScript.ProperName 'Language.PureScript.ConstructorName
         , [Language.PureScript.Type]
         )
       ]

instance Display WrongNewtypeConstructors where
  display = \case
    WrongNewtypeConstructors x [] ->
      "The newtype `"
        <> displayShow x
        <> "` has the wrong number of constructors or types."
        <> " Each newtype should have exactly one constructor"
        <> " and exactly one type."
        <> " This newtype has no constructors."
        <> " Add a constructor and a type."
    WrongNewtypeConstructors x y ->
      "The newtype `"
        <> displayShow x
        <> "` has the wrong number of constructors or types."
        <> " Each newtype should have exactly one constructor"
        <> " and exactly one type."
        <> " This newtype has `"
        <> display (length y)
        <> "` constructors `"
        <> displayList (fmap displayShow y)
        <> "`"
        <> " Ensure there is only one constructor and one type."
