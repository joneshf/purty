module Declaration
  ( Declarations(Declarations)
  , Errors
  , dynamic
  , fromPureScript
  , normalize
  , static
  ) where

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

normalizeData :: Data a -> Data Annotation.Normalized
normalizeData = \case
  Data name typeVariables alternates ->
    Data
      (Annotation.None <$ name)
      (Type.normalizeVariables typeVariables)
      ((fmap . fmap) normalizeAlternate alternates)

data Declaration a
  = DeclarationData !(Data a)
  | DeclarationNewtype !(Newtype a)
  deriving (Functor)

instance (Display a) => Display (Declaration a) where
  display = \case
    DeclarationData x -> "Declaration Data: " <> display x
    DeclarationNewtype x -> "Declaration Newtype: " <> display x

normalizeDeclaration :: Declaration a -> Declaration Annotation.Normalized
normalizeDeclaration = \case
  DeclarationData x -> DeclarationData (normalizeData x)
  DeclarationNewtype x -> DeclarationNewtype (normalizeNewtype x)

fromPureScript ::
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
  Language.PureScript.Declaration ->
  Eff e (Maybe (Declaration Annotation.Unannotated))
fromPureScript = \case
  Language.PureScript.BoundValueDeclaration {} -> pure Nothing
  Language.PureScript.DataDeclaration _ Language.PureScript.Data name' variables' constructors -> do
    alternates <- nonEmpty <$> traverse alternate constructors
    variables <- traverse (bitraverse (pure . Type.Variable) (traverse Kind.fromPureScript)) variables'
    let data' = Data name (Type.Variables $ nonEmpty variables) alternates
        name = Name.proper name'
    pure (Just $ DeclarationData data')
  Language.PureScript.DataDeclaration _ Language.PureScript.Newtype name' variables' [(constructor', [ty'])] -> do
    ty <- Type.fromPureScript ty'
    variables <- traverse (bitraverse (pure . Type.Variable) (traverse Kind.fromPureScript)) variables'
    let constructor = Name.constructor constructor'
        name = Name.proper name'
        newtype' =
          Newtype name (Type.Variables $ nonEmpty variables) constructor ty
    pure (Just $ DeclarationNewtype newtype')
  Language.PureScript.DataDeclaration _ Language.PureScript.Newtype name _ constructors ->
    throwError (WrongNewtypeConstructors name constructors)
  Language.PureScript.ExternDataDeclaration {} -> pure Nothing
  Language.PureScript.ExternDeclaration {} -> pure Nothing
  Language.PureScript.ExternKindDeclaration {} -> pure Nothing
  Language.PureScript.FixityDeclaration {} -> pure Nothing
  Language.PureScript.TypeClassDeclaration {} -> pure Nothing
  Language.PureScript.TypeDeclaration {} -> pure Nothing
  Language.PureScript.TypeInstanceDeclaration {} -> pure Nothing
  Language.PureScript.TypeSynonymDeclaration {} -> pure Nothing
  Language.PureScript.ValueDeclaration {} -> pure Nothing
  Language.PureScript.BindingGroupDeclaration {} -> pure Nothing
  Language.PureScript.DataBindingGroupDeclaration {} -> pure Nothing
  Language.PureScript.ImportDeclaration {} -> pure Nothing

newtype Declarations a
  = Declarations (Maybe (NonEmpty (Declaration a)))

instance (Display a) => Display (Declarations a) where
  display = \case
    Declarations Nothing -> "No Declarations"
    Declarations (Just declarations) ->
      "Declarations ["
        <> intercalateMap1 ", " display declarations
        <> "]"

normalize :: Declarations a -> Declarations Annotation.Normalized
normalize = \case
  Declarations declarations' ->
    Declarations ((fmap . fmap) normalizeDeclaration declarations')

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

normalizeNewtype :: Newtype a -> Newtype Annotation.Normalized
normalizeNewtype = \case
  Newtype name variables constructor type'' ->
    Newtype
      (Annotation.None <$ name)
      (Type.normalizeVariables variables)
      (Annotation.None <$ constructor)
      (Type.normalize type'')

dynamic, static :: Declarations Annotation.Normalized -> Doc a
(dynamic, static) = (dynamic', static')
  where
  dynamic' = \case
    Declarations Nothing -> mempty
    Declarations (Just declarations) ->
      line
        <> line
        <> intercalateMap1 line go declarations
      where
      go = \case
        DeclarationData (Data name variables (Just alternates)) ->
          "data" <+> Name.docFromProper name <> Type.docFromVariables variables
            <> line <> indent 2 (align doc)
            where
            doc =
              equals
                <+> intercalateMap1
                  (line <> pipe <> space)
                  docFromAlternate
                  alternates
        DeclarationData (Data name variables Nothing) ->
          "data" <+> Name.docFromProper name <> Type.docFromVariables variables
        DeclarationNewtype (Newtype name variables constructor type'') ->
          "newtype" <+> Name.docFromProper name <> Type.docFromVariables variables
            <> line <> indent 2 (equals <+> docConstructor <+> docType)
            where
            docConstructor = Name.docFromConstructor constructor
            docType = Type.doc type''
  static' = \case
    Declarations Nothing -> mempty
    Declarations (Just declarations) ->
      line
        <> line
        <> intercalateMap1 line go declarations
      where
      go = \case
        DeclarationData (Data name variables (Just alternates)) ->
          "data" <+> Name.docFromProper name <> Type.docFromVariables variables
            <> line <> indent 2 (align doc)
            where
            doc =
              equals
                <+> intercalateMap1
                  (line <> pipe <> space)
                  docFromAlternate
                  alternates
        DeclarationData (Data name variables Nothing) ->
          "data" <+> Name.docFromProper name <> Type.docFromVariables variables
        DeclarationNewtype (Newtype name variables constructor type'') ->
          "newtype" <+> Name.docFromProper name <> Type.docFromVariables variables
            <> line <> indent 2 (equals <+> docConstructor <+> docType)
            where
            docConstructor = Name.docFromConstructor constructor
            docType = Type.doc type''

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
