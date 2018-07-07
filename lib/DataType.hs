module DataType where

import "rio" RIO hiding (Data)

import "freer-simple" Control.Monad.Freer        (Eff, Members)
import "freer-simple" Control.Monad.Freer.Error  (Error, throwError)
import "base" Data.Bitraversable                 (bitraverse)
import "base" Data.List                          (intersperse)
import "base" Data.List.NonEmpty                 (NonEmpty, nonEmpty)
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

import qualified "purescript" Language.PureScript

import qualified "this" Annotation
import qualified "this" Kind
import qualified "this" Name
import qualified "this" Type

data Alternate a
  = Alternate !a !(Name.Constructor a) !(Maybe (NonEmpty (Type.Type a)))
  deriving (Functor)

instance (Display a) => Display (Alternate a) where
  display = \case
    Alternate x y z' ->
      "Alternate: "
        <> "annotation: "
        <> display x
        <> ", constructor: "
        <> display y
        <> foldMap (\z -> ", types: [" <> intercalateMap1 ", " display z <> "]") z'

alternate ::
  ( Members
    '[ Error WrongNewtypeConstructors
     , Error Kind.InferredKind
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
  ( Language.PureScript.ProperName 'Language.PureScript.ConstructorName
  , [Language.PureScript.Type]
  ) ->
  Eff e (Alternate Annotation.Unannotated)
alternate = \case
  (x', y') -> do
    let x = Name.constructor x'
    y <- nonEmpty <$> traverse Type.fromPureScript y'
    pure (Alternate Annotation.Unannotated x y)

docFromAlternate :: Alternate Annotation.Normalized -> Doc a
docFromAlternate = \case
  Alternate ann x y -> annotate doc
    where
    annotate = case ann of
      Annotation.None   -> id
      Annotation.Braces -> braces
      Annotation.Parens -> parens
    doc =
      Name.docFromConstructor x
        <> foldMap (\types -> space <> intercalateMap1 space Type.doc types) y

normalizeAlternate :: Alternate a -> Alternate Annotation.Normalized
normalizeAlternate = \case
  Alternate _ann x y ->
    Alternate
      Annotation.None
      (Annotation.None <$ x)
      ((fmap . fmap) Type.normalize y)

data Data a
  = Data !(Name.Proper a) !(Type.Variables a) !(Maybe (NonEmpty (Alternate a)))
  deriving (Functor)

instance (Display a) => Display (Data a) where
  display = \case
    Data x y z' ->
      "Data"
        <> " name:"
        <> display x
        <> ", variables:"
        <> display y
        <> foldMap
          (\z -> ", alternates: [" <> intercalateMap1 ", " display z <> "]")
          z'

data' ::
  ( Members
    '[ Error WrongNewtypeConstructors
     , Error Kind.InferredKind
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
  Language.PureScript.ProperName 'Language.PureScript.TypeName ->
  [(Text, Maybe Language.PureScript.Kind)] ->
  [ ( Language.PureScript.ProperName 'Language.PureScript.ConstructorName
    , [ Language.PureScript.Type
      ]
    )
  ] ->
  Eff e (Data Annotation.Unannotated)
data' name variables' constructors = do
  alternates <- nonEmpty <$> traverse alternate constructors
  variables <- traverse (bitraverse (pure . Type.Variable) (traverse Kind.fromPureScript)) variables'
  pure (Data (Name.proper name) (Type.Variables $ nonEmpty variables) alternates)

docFromData :: Data Annotation.Normalized -> Doc a
docFromData = \case
  Data name variables (Just alternates) ->
    "data" <+> Name.docFromProper name <> Type.docFromVariables variables
      <> line <> indent 2 (align doc)
      <> line
      where
      doc =
        equals
          <+> intercalateMap1 (line <> pipe <> space) docFromAlternate alternates
  Data name variables Nothing ->
    "data" <+> Name.docFromProper name <> Type.docFromVariables variables
      <> line

normalizeData :: Data a -> Data Annotation.Normalized
normalizeData = \case
  Data name typeVariables alternates ->
    Data
      (Annotation.None <$ name)
      (Type.normalizeVariables typeVariables)
      ((fmap . fmap) normalizeAlternate alternates)

data Newtype a
  = Newtype
    !(Name.Proper a)
    !(Type.Variables a)
    !(Name.Constructor a)
    !(Type.Type a)
  deriving (Functor)

instance (Display a) => Display (Newtype a) where
  display = \case
    Newtype name variables constructor type'' ->
      "Newtype"
        <> " name: "
        <> display name
        <> ", variables:"
        <> display variables
        <> ", constuctor:"
        <> display constructor
        <> ", type:"
        <> display type''

docFromNewtype :: Newtype Annotation.Normalized -> Doc a
docFromNewtype = \case
  Newtype name variables constructor type'' ->
    "newtype" <+> Name.docFromProper name <> Type.docFromVariables variables
      <> line <> indent 2 (equals <+> docConstructor <+> docType)
      <> line
      where
      docConstructor = Name.docFromConstructor constructor
      docType = Type.doc type''

newtype' ::
  ( Members
    '[ Error WrongNewtypeConstructors
     , Error Kind.InferredKind
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
  Language.PureScript.ProperName 'Language.PureScript.TypeName ->
  [(Text, Maybe Language.PureScript.Kind)] ->
  [ ( Language.PureScript.ProperName 'Language.PureScript.ConstructorName
    , [ Language.PureScript.Type
      ]
    )
  ] ->
  Eff e (Newtype Annotation.Unannotated)
newtype' name' variables' = \case
  [(constructor', [ty'])] -> do
    ty <- Type.fromPureScript ty'
    variables <-
      traverse
        (bitraverse (pure . Type.Variable) (traverse Kind.fromPureScript))
        variables'
    let constructor = Name.constructor constructor'
        name = Name.proper name'
    pure (Newtype name (Type.Variables $ nonEmpty variables) constructor ty)
  constructors ->
    throwError (WrongNewtypeConstructors name' constructors)

normalizeNewtype :: Newtype a -> Newtype Annotation.Normalized
normalizeNewtype = \case
  Newtype name variables constructor type'' ->
    Newtype
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
