module Declaration where

import "rio" RIO hiding (Data)

import "freer-simple" Control.Monad.Freer        (Eff, Members)
import "freer-simple" Control.Monad.Freer.Error  (Error, throwError)
import "base" Data.Bitraversable                 (bitraverse)
import "base" Data.List                          (intersperse)
import "base" Data.List.NonEmpty                 (NonEmpty((:|)), nonEmpty)
import "semigroupoids" Data.Semigroup.Foldable   (intercalateMap1)
import "prettyprinter" Data.Text.Prettyprint.Doc
    ( Doc
    , align
    , braces
    , colon
    , dot
    , equals
    , indent
    , line
    , lparen
    , parens
    , pipe
    , pretty
    , rparen
    , space
    , (<+>)
    )
import "purescript" Language.PureScript.PSString (PSString)

import qualified "purescript" Language.PureScript
import qualified "purescript" Language.PureScript.Label

import qualified "this" Annotation
import qualified "this" Kind
import qualified "this" Name

data Alternate a
  = Alternate !a !(Name.Constructor a) !(Maybe (NonEmpty (Type a)))
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
        <> foldMap (\types -> space <> intercalateMap1 space docFromType types) y

alternate ::
  ( Members
    '[ Error InferredConstraintData
     , Error InferredForallWithSkolem
     , Error InferredSkolem
     , Error InferredType
     , Error InfixTypeNotTypeOp
     , Error PrettyPrintForAll
     , Error PrettyPrintFunction
     , Error PrettyPrintObject
     , Error Kind.InferredKind
     , Error Name.Missing
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
    y <- nonEmpty <$> traverse type' y'
    pure (Alternate Annotation.Unannotated x y)

normalizeAlternate :: Alternate a -> Alternate Annotation.Normalized
normalizeAlternate = \case
  Alternate _ann x y ->
    Alternate
      Annotation.None
      (Annotation.None <$ x)
      ((fmap . fmap) normalizeType y)

data Constraint a
  = Constraint !(Name.Qualified Name.Class a) !(Maybe (NonEmpty (Type a)))
  deriving (Functor)

instance (Display a) => Display (Constraint a) where
  display = \case
    Constraint x y' ->
      "Constraint: "
        <> "class: "
        <> display x
        <> foldMap (\y -> ", types: [" <> intercalateMap1 ", " display y <> "]") y'

constraint ::
  ( Members
    '[ Error InferredConstraintData
     , Error InferredForallWithSkolem
     , Error InferredSkolem
     , Error InferredType
     , Error InfixTypeNotTypeOp
     , Error PrettyPrintForAll
     , Error PrettyPrintFunction
     , Error PrettyPrintObject
     , Error Kind.InferredKind
     , Error Name.Missing
     ]
    e
  ) =>
  Language.PureScript.Constraint ->
  Eff e (Constraint Annotation.Unannotated)
constraint = \case
  Language.PureScript.Constraint x y (Just z) ->
    throwError (InferredConstraintData x y z)
  Language.PureScript.Constraint x' y' Nothing -> do
    x <- Name.qualified (pure . Name.class') x'
    y <- nonEmpty <$> traverse type' y'
    pure (Constraint x y)

docFromConstraint :: Constraint Annotation.Normalized -> Doc b
docFromConstraint = \case
  Constraint x Nothing -> Name.docFromQualified Name.docFromClass x
  Constraint x (Just y) ->
    Name.docFromQualified Name.docFromClass x
      <+> intercalateMap1 space docFromType y

normalizeConstraint :: Constraint a -> Constraint Annotation.Normalized
normalizeConstraint = \case
  Constraint x y ->
    Constraint (Annotation.None <$ x) ((fmap . fmap) normalizeType y)

data Data a
  = Data !(Name.Proper a) !(TypeVariables a) !(Maybe (NonEmpty (Alternate a)))
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
      (normalizeTypeVariables typeVariables)
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
    '[ Error InferredConstraintData
     , Error InferredForallWithSkolem
     , Error InferredSkolem
     , Error InferredType
     , Error InfixTypeNotTypeOp
     , Error PrettyPrintForAll
     , Error PrettyPrintFunction
     , Error PrettyPrintObject
     , Error WrongNewtypeConstructors
     , Error Kind.InferredKind
     , Error Name.Missing
     ]
    e
  ) =>
  Language.PureScript.Declaration ->
  Eff e (Maybe (Declaration Annotation.Unannotated))
fromPureScript = \case
  Language.PureScript.BoundValueDeclaration {} -> pure Nothing
  Language.PureScript.DataDeclaration _ Language.PureScript.Data name' variables' constructors -> do
    alternates <- nonEmpty <$> traverse alternate constructors
    variables <- traverse (bitraverse (pure . TypeVariable) (traverse Kind.fromPureScript)) variables'
    let data' = Data name (TypeVariables $ nonEmpty variables) alternates
        name = Name.proper name'
    pure (Just $ DeclarationData data')
  Language.PureScript.DataDeclaration _ Language.PureScript.Newtype name' variables' [(constructor', [ty'])] -> do
    ty <- type' ty'
    variables <- traverse (bitraverse (pure . TypeVariable) (traverse Kind.fromPureScript)) variables'
    let constructor = Name.constructor constructor'
        name = Name.proper name'
        newtype' =
          Newtype name (TypeVariables $ nonEmpty variables) constructor ty
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

newtype Forall
  = Forall Text

instance Display Forall where
  display = \case
    Forall x ->
      "Forall: "
        <> display x

docFromForall :: Forall -> Doc a
docFromForall = \case
  Forall x -> "forall" <+> pretty x <> dot

-- |
-- We're using the underlying PureScript representation here,
-- as it handles unicode properly for the language.
newtype Label
  = Label PSString

instance Display Label where
  display = \case
    Label x ->
      "Label: "
        <> displayShow x

docFromLabel :: Label -> Doc a
docFromLabel = \case
  Label x -> pretty (Language.PureScript.prettyPrintString x)

label :: Language.PureScript.Label.Label -> Label
label = \case
  Language.PureScript.Label.Label x -> Label x

data Newtype a
  = Newtype !(Name.Proper a) !(TypeVariables a) !(Name.Constructor a) !(Type a)
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
      (normalizeTypeVariables variables)
      (Annotation.None <$ constructor)
      (normalizeType type'')

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
          "data" <+> Name.docFromProper name <> docFromTypeVariables variables
            <> line <> indent 2 (align doc)
            where
            doc =
              equals
                <+> intercalateMap1
                  (line <> pipe <> space)
                  docFromAlternate
                  alternates
        DeclarationData (Data name variables Nothing) ->
          "data" <+> Name.docFromProper name <> docFromTypeVariables variables
        DeclarationNewtype (Newtype name variables constructor type'') ->
          "newtype" <+> Name.docFromProper name <> docFromTypeVariables variables
            <> line <> indent 2 (equals <+> docConstructor <+> docType)
            where
            docConstructor = Name.docFromConstructor constructor
            docType = docFromType type''
  static' = \case
    Declarations Nothing -> mempty
    Declarations (Just declarations) ->
      line
        <> line
        <> intercalateMap1 line go declarations
      where
      go = \case
        DeclarationData (Data name variables (Just alternates)) ->
          "data" <+> Name.docFromProper name <> docFromTypeVariables variables
            <> line <> indent 2 (align doc)
            where
            doc =
              equals
                <+> intercalateMap1
                  (line <> pipe <> space)
                  docFromAlternate
                  alternates
        DeclarationData (Data name variables Nothing) ->
          "data" <+> Name.docFromProper name <> docFromTypeVariables variables
        DeclarationNewtype (Newtype name variables constructor type'') ->
          "newtype" <+> Name.docFromProper name <> docFromTypeVariables variables
            <> line <> indent 2 (equals <+> docConstructor <+> docType)
            where
            docConstructor = Name.docFromConstructor constructor
            docType = docFromType type''

data Row a
  = RowAnnotation !a !(Row a)
  | RowCons !Label !(Type a) !(Type a)
  | RowEmpty
  deriving (Functor)

instance (Display a) => Display (Row a) where
  display = \case
    RowAnnotation x y ->
      "Row Annotation: "
        <> "annotation: "
        <> display x
        <> ", row: "
        <> display y
    RowCons x y z ->
      "Row Cons: "
        <> "label: "
        <> display x
        <> ", type: "
        <> display y
        <> ", tail: "
        <> display z
    RowEmpty ->
      "Row Empty"

docFromRow :: Row Annotation.Normalized -> Doc a
docFromRow = \case
  RowAnnotation Annotation.None x -> docFromRow x
  RowAnnotation Annotation.Braces x -> braces (docFromRow x)
  RowAnnotation Annotation.Parens x -> parens (docFromRow x)
  RowEmpty -> lparen <> rparen
  RowCons x y z -> docFromLabel x <+> docFromType y <+> pipe <+> docFromType z

normalizeRow :: Row a -> Row Annotation.Normalized
normalizeRow = \case
  RowAnnotation _ann x -> normalizeRow x
  RowCons x y z -> RowCons x (normalizeType y) (normalizeType z)
  RowEmpty -> RowEmpty

-- |
-- We're using the underlying PureScript representation here,
-- as it handles unicode properly for the language.
newtype Symbol
  = Symbol PSString

instance Display Symbol where
  display = \case
    Symbol x ->
      "Symbol: "
        <> displayShow x

docFromSymbol :: Symbol -> Doc a
docFromSymbol = \case
  Symbol x -> pretty (Language.PureScript.prettyPrintString x)

data Type a
  = TypeAnnotation !a !(Type a)
  | TypeApplication !(Type a) !(Type a)
  | TypeConstrained !(Constraint a) !(Type a)
  | TypeForall !Forall !(Type a)
  | TypeInfixOperator !(Type a) !(Name.Qualified Name.TypeOperator a) !(Type a)
  | TypeKinded !(Type a) !(Kind.Kind a)
  | TypeRow !(Row a)
  | TypeParens !(Type a)
  | TypeSymbol !Symbol
  | TypeTypeConstructor !(Name.Qualified Name.TypeConstructor a)
  | TypeTypeOperator !(Name.Qualified Name.TypeOperator a)
  | TypeTypeVariable !TypeVariable
  | TypeWildcard !Wildcard
  deriving (Functor)

instance (Display a) => Display (Type a) where
  display = \case
    TypeAnnotation ann x ->
      "Type Annotation: "
        <> "annotation: "
        <> display ann
        <> ", type: "
        <> display x
    TypeApplication x y ->
      "Type Application: "
        <> "left: "
        <> display x
        <> ", right: "
        <> display y
    TypeConstrained x y ->
      "Type Constrained: "
        <> "constraint: "
        <> display x
        <> ", type: "
        <> display y
    TypeForall x y ->
      "Type Forall: "
        <> "forall: "
        <> display x
        <> ", type: "
        <> display y
    TypeInfixOperator x y z ->
      "Type Infix Operator: "
        <> "left: "
        <> display x
        <> ", operator: "
        <> display y
        <> ", right: "
        <> display z
    TypeKinded x y ->
      "Type Kinded: "
        <> "type: "
        <> display x
        <> ", kind: "
        <> display y
    TypeParens x ->
      "Type Parens: "
        <> "type: "
        <> display x
    TypeRow x ->
      "Type Row: "
        <> "row: "
        <> display x
    TypeSymbol x ->
      "Type Symbol: "
        <> "symbol: "
        <> display x
    TypeTypeConstructor x ->
      "Type Constructor: "
        <> "constructor: "
        <> display x
    TypeTypeOperator x ->
      "Type Operator: "
        <> "operator: "
        <> display x
    TypeTypeVariable x ->
      "Type Variable: "
        <> "variable: "
        <> display x
    TypeWildcard x ->
      "Type Wildcard: "
        <> "wildcard: "
        <> display x

normalizeTypeApplication :: Type a -> Type b -> Type Annotation.Normalized
normalizeTypeApplication x y = case (x, y) of
  ( TypeTypeConstructor
      ( Name.Qualified
          (Just (Name.Module (Name.Proper _ "Prim" :| [])))
          (Name.TypeConstructor (Name.Proper _ "Record"))
      )
    , TypeRow {}
    ) -> TypeAnnotation Annotation.Braces (normalizeType y)
  (_, _) -> TypeApplication (normalizeType x) (normalizeType y)

docFromType :: Type Annotation.Normalized -> Doc b
docFromType = \case
  TypeAnnotation Annotation.None x -> docFromType x
  TypeAnnotation Annotation.Braces x -> braces (docFromType x)
  TypeAnnotation Annotation.Parens x -> parens (docFromType x)
  TypeApplication x y -> docFromType x <+> docFromType y
  TypeConstrained x y -> docFromConstraint x <+> "=>" <+> docFromType y
  TypeForall x y -> docFromForall x <+> docFromType y
  TypeInfixOperator x y z -> docFromType x <+> Name.docFromQualified Name.docFromTypeOperator y <+> docFromType z
  TypeKinded x y -> docFromType x <+> colon <> colon <+> Kind.doc y
  TypeRow x -> docFromRow x
  TypeParens x -> docFromType (TypeAnnotation Annotation.Parens x)
  TypeSymbol x -> docFromSymbol x
  TypeTypeConstructor x -> Name.docFromQualified Name.docFromTypeConstructor x
  TypeTypeOperator x -> parens (Name.docFromQualified Name.docFromTypeOperator x)
  TypeTypeVariable x -> docFromTypeVariable x
  TypeWildcard x -> docFromWildcard x

normalizeType :: Type a -> Type Annotation.Normalized
normalizeType = \case
  TypeAnnotation _ann x -> normalizeType x
  TypeApplication x y -> normalizeTypeApplication x y
  TypeConstrained x y ->
    TypeConstrained (normalizeConstraint x) (normalizeType y)
  TypeForall x y -> TypeForall x (normalizeType y)
  TypeInfixOperator x y z ->
    TypeInfixOperator (normalizeType x) (Annotation.None <$ y) (normalizeType z)
  TypeKinded x y -> TypeKinded (normalizeType x) (Kind.normalize y)
  TypeRow x -> TypeRow (normalizeRow x)
  TypeParens x -> TypeAnnotation Annotation.Parens (normalizeType x)
  TypeSymbol x -> TypeSymbol x
  TypeTypeConstructor x -> TypeTypeConstructor (Annotation.None <$ x)
  TypeTypeOperator x -> TypeTypeOperator (Annotation.None <$ x)
  TypeTypeVariable x -> TypeTypeVariable x
  TypeWildcard x -> TypeWildcard x

type' ::
  ( Members
    '[ Error InferredConstraintData
     , Error InferredForallWithSkolem
     , Error InferredSkolem
     , Error InferredType
     , Error InfixTypeNotTypeOp
     , Error PrettyPrintForAll
     , Error PrettyPrintFunction
     , Error PrettyPrintObject
     , Error Kind.InferredKind
     , Error Name.Missing
     ]
    e
  ) =>
  Language.PureScript.Type ->
  Eff e (Type Annotation.Unannotated)
type' = \case
  Language.PureScript.TUnknown _ -> throwError InferredType
  Language.PureScript.TypeVar x -> pure (TypeTypeVariable $ TypeVariable x)
  Language.PureScript.TypeLevelString x -> pure (TypeSymbol $ Symbol x)
  Language.PureScript.TypeWildcard _ -> pure (TypeWildcard Wildcard)
  Language.PureScript.TypeConstructor x ->
    fmap TypeTypeConstructor (Name.qualified (pure . Name.typeConstructor) x)
  Language.PureScript.TypeOp x ->
    fmap TypeTypeOperator (Name.qualified (pure . Name.typeOperator) x)
  Language.PureScript.TypeApp x y -> TypeApplication <$> type' x <*> type' y
  Language.PureScript.ForAll x y Nothing ->
    fmap (TypeForall $ Forall x) (type' y)
  Language.PureScript.ForAll x y (Just z) ->
    throwError (InferredForallWithSkolem x y z)
  Language.PureScript.ConstrainedType x y ->
    TypeConstrained <$> constraint x <*> type' y
  Language.PureScript.Skolem w x y z -> throwError (InferredSkolem w x y z)
  Language.PureScript.REmpty -> pure (TypeRow RowEmpty)
  Language.PureScript.RCons x y z ->
    fmap TypeRow (RowCons (label x) <$> type' y <*> type' z)
  Language.PureScript.KindedType x y ->
    TypeKinded <$> type' x <*> Kind.fromPureScript y
  Language.PureScript.PrettyPrintFunction x y ->
    throwError (PrettyPrintFunction x y)
  Language.PureScript.PrettyPrintObject x -> throwError (PrettyPrintObject x)
  Language.PureScript.PrettyPrintForAll x y -> throwError (PrettyPrintForAll x y)
  Language.PureScript.BinaryNoParensType (Language.PureScript.TypeOp x') y' z' -> do
    x <- Name.qualified (pure . Name.typeOperator) x'
    y <- type' y'
    z <- type' z'
    pure (TypeInfixOperator y x z)
  Language.PureScript.BinaryNoParensType x y z ->
    throwError (InfixTypeNotTypeOp x y z)
  Language.PureScript.ParensInType x -> fmap TypeParens (type' x)

newtype TypeVariable
  = TypeVariable Text

instance Display TypeVariable where
  display = \case
    TypeVariable x -> "Type Variable: " <> display x

docFromTypeVariable :: TypeVariable -> Doc a
docFromTypeVariable = \case
  TypeVariable x -> pretty x

newtype TypeVariables a
  = TypeVariables (Maybe (NonEmpty (TypeVariable, Maybe (Kind.Kind a))))
  deriving (Functor)

instance (Display a) => Display (TypeVariables a) where
  display = \case
    TypeVariables Nothing -> "No Type Variables"
    TypeVariables (Just variables) ->
      "Type Variables: ["
        <> intercalateMap1 ", " go variables
        <> "]"
        where
        go = \case
          (typeVariable, Nothing) ->
            "Type Variable: "
              <> display typeVariable
              <> ", No Kind"
          (typeVariable, Just x) ->
            "Type Variable: "
              <> display typeVariable
              <> ", Kind: "
              <> display x

docFromTypeVariables :: TypeVariables Annotation.Normalized -> Doc b
docFromTypeVariables = \case
  TypeVariables Nothing -> mempty
  TypeVariables (Just variables) -> space <> intercalateMap1 space go variables
    where
    go = \case
      (typeVariable, Nothing) -> docFromTypeVariable typeVariable
      (typeVariable, Just kind') -> parens doc
        where
        doc =
          docFromTypeVariable typeVariable
            <+> colon
            <> colon
            <+> Kind.doc kind'

normalizeTypeVariables :: TypeVariables a -> TypeVariables Annotation.Normalized
normalizeTypeVariables = \case
  TypeVariables variables ->
    TypeVariables ((fmap . fmap . fmap . fmap) Kind.normalize variables)

data Wildcard
  = Wildcard

instance Display Wildcard where
  display = \case
    Wildcard ->
      "Wildcard"

docFromWildcard :: Wildcard -> Doc a
docFromWildcard = \case
  Wildcard -> "_"

displayList :: Display a => [a] -> Utf8Builder
displayList xs = "[" <> fold (intersperse ", " (display <$> xs)) <> "]"

-- Errors

type Errors
  = '[ Error InferredConstraintData
     , Error InferredForallWithSkolem
     , Error InferredSkolem
     , Error InferredType
     , Error InfixTypeNotTypeOp
     , Error PrettyPrintForAll
     , Error PrettyPrintFunction
     , Error PrettyPrintObject
     , Error WrongNewtypeConstructors
     ]

data InferredConstraintData
  = InferredConstraintData
      !( Language.PureScript.Qualified
           (Language.PureScript.ProperName 'Language.PureScript.ClassName)
       )
      ![Language.PureScript.Type]
      !Language.PureScript.ConstraintData

data InferredForallWithSkolem
  = InferredForallWithSkolem
      !Text
      !Language.PureScript.Type
      !Language.PureScript.SkolemScope

data InferredSkolem
  = InferredSkolem
      !Text
      !Int
      !Language.PureScript.SkolemScope
      !(Maybe Language.PureScript.SourceSpan)

data InferredType
  = InferredType

data InfixTypeNotTypeOp
  = InfixTypeNotTypeOp
      !Language.PureScript.Type
      !Language.PureScript.Type
      !Language.PureScript.Type

data PrettyPrintForAll
  = PrettyPrintForAll ![Text] !Language.PureScript.Type

data PrettyPrintFunction
  = PrettyPrintFunction !Language.PureScript.Type !Language.PureScript.Type

newtype PrettyPrintObject
  = PrettyPrintObject Language.PureScript.Type

data WrongNewtypeConstructors
  = WrongNewtypeConstructors
      !(Language.PureScript.ProperName 'Language.PureScript.TypeName)
      ![ ( Language.PureScript.ProperName 'Language.PureScript.ConstructorName
         , [Language.PureScript.Type]
         )
       ]

instance Display InferredConstraintData where
  display = \case
    InferredConstraintData x y z ->
      "The compiler inferred metadata `"
        <> displayShow z
        <> "` for the constraint `"
        <> displayShow x
        <> " => "
        <> displayShow y
        <> "` There should be no constraint metadata at this point."
        <> " We are either using the wrong function from the PureScript library,"
        <> " or there's a problem in the PureScript library."

instance Display InferredForallWithSkolem where
  display = \case
    InferredForallWithSkolem x y z ->
      "The compiler inferred a skolem `"
        <> displayShow z
        <> "` for the forall `forall "
        <> display x
        <> ". "
        <> displayShow y
        <> "`. There should be no skolems at this point."
        <> " We are either using the wrong function from the PureScript library,"
        <> " or there's a problem in the PureScript library."

instance Display InferredSkolem where
  display = \case
    InferredSkolem w x y z' ->
      "The compiler inferred a skolem `"
        <> display x
        <> "` for the type variable `"
        <> display w
        <> "` with scope `"
        <> displayShow y
        <> foldMap (\z -> "` at `" <> displayShow z) z'
        <> "`. There should be no skolems at this point."
        <> " We are either using the wrong function from the PureScript library,"
        <> " or there's a problem in the PureScript library."

instance Display InferredType where
  display = \case
    InferredType ->
      "The compiler inferred a type."
        <> " But, only types in the source file should exist at this point."
        <> " We are either using the wrong function from the PureScript library,"
        <> " or there's a problem in the PureScript library."

instance Display InfixTypeNotTypeOp where
  display = \case
    InfixTypeNotTypeOp x y z ->
      "We do not handle the case where the infix type is `"
        <> displayShow x
        <> "`. The left side is `"
        <> displayShow y
        <> "`. The right side is `"
        <> displayShow z
        <> "`. If the infix type contains a `TypeOp` somewhere inside of it,"
        <> " we should handle that case appropriately."
        <> " Otherwise, this seems like a problem in the PureScript library."

instance Display PrettyPrintForAll where
  display = \case
    PrettyPrintForAll x' y ->
      "We tried to modify foralls using a function from the PureScript library."
        <> " We ended up with `forall"
        <> foldMap (\x -> " " <> display x) x'
        <> "."
        <> displayShow y
        <> "`. We should handle modifying foralls ourselves."

instance Display PrettyPrintFunction where
  display = \case
    PrettyPrintFunction x y ->
      "We tried to modify function types using a function"
        <> " from the PureScript library."
        <> " We ended up with `"
        <> displayShow x
        <> " -> "
        <> displayShow y
        <> "`. We should handle modifying function types ourselves."

instance Display PrettyPrintObject where
  display = \case
    PrettyPrintObject x ->
      "We tried to modify a record type using a function"
        <> " from the PureScript library."
        <> " We ended up with `"
        <> displayShow x
        <> "`. We should handle modifying record types ourselves."

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
