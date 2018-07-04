module Type
  ( Errors
  , Type
  , Variable(Variable)
  , Variables(Variables)
  , InferredConstraintData
  , InferredForallWithSkolem
  , InferredSkolem
  , InferredType
  , InfixTypeNotTypeOp
  , PrettyPrintForAll
  , PrettyPrintFunction
  , PrettyPrintObject
  , doc
  , docFromVariables
  , fromPureScript
  , normalize
  , normalizeVariables
  ) where

import "rio" RIO hiding (Data)

import "freer-simple" Control.Monad.Freer        (Eff, Members)
import "freer-simple" Control.Monad.Freer.Error  (Error, throwError)
import "base" Data.List.NonEmpty                 (NonEmpty((:|)), nonEmpty)
import "semigroupoids" Data.Semigroup.Foldable   (intercalateMap1)
import "prettyprinter" Data.Text.Prettyprint.Doc
    ( Doc
    , braces
    , colon
    , dot
    , lbrace
    , lparen
    , parens
    , pipe
    , pretty
    , rbrace
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
    y <- nonEmpty <$> traverse fromPureScript y'
    pure (Constraint x y)

docFromConstraint :: Constraint Annotation.Normalized -> Doc b
docFromConstraint = \case
  Constraint x Nothing -> Name.docFromQualified Name.docFromClass x
  Constraint x (Just y) ->
    Name.docFromQualified Name.docFromClass x <+> intercalateMap1 space doc y

normalizeConstraint :: Constraint a -> Constraint Annotation.Normalized
normalizeConstraint = \case
  Constraint x y ->
    Constraint (Annotation.None <$ x) ((fmap . fmap) normalize y)

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
  RowAnnotation Annotation.Braces RowEmpty -> lbrace <> rbrace
  RowAnnotation Annotation.Braces x -> braces (docFromRow x)
  RowAnnotation Annotation.Parens RowEmpty -> lparen <> rparen
  RowAnnotation Annotation.Parens x -> parens (docFromRow x)
  RowEmpty -> lparen <> rparen
  RowCons x y z -> docFromLabel x <+> doc y <+> pipe <+> doc z

normalizeRow :: Row a -> Row Annotation.Normalized
normalizeRow = \case
  RowAnnotation _ann x -> normalizeRow x
  RowCons x y z -> RowCons x (normalize y) (normalize z)
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
  | TypeVariable !Variable
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
    TypeVariable x ->
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
    , TypeRow row
    ) -> TypeRow (RowAnnotation Annotation.Braces (normalizeRow row))
  (_, _) -> TypeApplication (normalize x) (normalize y)

doc :: Type Annotation.Normalized -> Doc b
doc = \case
  TypeAnnotation Annotation.None x -> doc x
  TypeAnnotation Annotation.Braces x -> braces (doc x)
  TypeAnnotation Annotation.Parens x -> parens (doc x)
  TypeApplication x y -> doc x <+> doc y
  TypeConstrained x y -> docFromConstraint x <+> "=>" <+> doc y
  TypeForall x y -> docFromForall x <+> doc y
  TypeInfixOperator x y z -> doc x <+> Name.docFromQualified Name.docFromTypeOperator y <+> doc z
  TypeKinded x y -> doc x <+> colon <> colon <+> Kind.doc y
  TypeRow x -> docFromRow x
  TypeParens x -> doc (TypeAnnotation Annotation.Parens x)
  TypeSymbol x -> docFromSymbol x
  TypeTypeConstructor x -> Name.docFromQualified Name.docFromTypeConstructor x
  TypeTypeOperator x -> parens (Name.docFromQualified Name.docFromTypeOperator x)
  TypeVariable x -> docFromVariable x
  TypeWildcard x -> docFromWildcard x

normalize :: Type a -> Type Annotation.Normalized
normalize = \case
  TypeAnnotation _ann x -> normalize x
  TypeApplication x y -> normalizeTypeApplication x y
  TypeConstrained x y ->
    TypeConstrained (normalizeConstraint x) (normalize y)
  TypeForall x y -> TypeForall x (normalize y)
  TypeInfixOperator x y z ->
    TypeInfixOperator (normalize x) (Annotation.None <$ y) (normalize z)
  TypeKinded x y -> TypeKinded (normalize x) (Kind.normalize y)
  TypeRow x -> TypeRow (normalizeRow x)
  TypeParens x -> TypeAnnotation Annotation.Parens (normalize x)
  TypeSymbol x -> TypeSymbol x
  TypeTypeConstructor x -> TypeTypeConstructor (Annotation.None <$ x)
  TypeTypeOperator x -> TypeTypeOperator (Annotation.None <$ x)
  TypeVariable x -> TypeVariable x
  TypeWildcard x -> TypeWildcard x

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
     , Error Kind.InferredKind
     , Error Name.Missing
     ]
    e
  ) =>
  Language.PureScript.Type ->
  Eff e (Type Annotation.Unannotated)
fromPureScript = \case
  Language.PureScript.TUnknown _ -> throwError InferredType
  Language.PureScript.TypeVar x -> pure (TypeVariable $ Variable x)
  Language.PureScript.TypeLevelString x -> pure (TypeSymbol $ Symbol x)
  Language.PureScript.TypeWildcard _ -> pure (TypeWildcard Wildcard)
  Language.PureScript.TypeConstructor x ->
    fmap TypeTypeConstructor (Name.qualified (pure . Name.typeConstructor) x)
  Language.PureScript.TypeOp x ->
    fmap TypeTypeOperator (Name.qualified (pure . Name.typeOperator) x)
  Language.PureScript.TypeApp x y -> TypeApplication <$> fromPureScript x <*> fromPureScript y
  Language.PureScript.ForAll x y Nothing ->
    fmap (TypeForall $ Forall x) (fromPureScript y)
  Language.PureScript.ForAll x y (Just z) ->
    throwError (InferredForallWithSkolem x y z)
  Language.PureScript.ConstrainedType x y ->
    TypeConstrained <$> constraint x <*> fromPureScript y
  Language.PureScript.Skolem w x y z -> throwError (InferredSkolem w x y z)
  Language.PureScript.REmpty -> pure (TypeRow RowEmpty)
  Language.PureScript.RCons x y z ->
    fmap TypeRow (RowCons (label x) <$> fromPureScript y <*> fromPureScript z)
  Language.PureScript.KindedType x y ->
    TypeKinded <$> fromPureScript x <*> Kind.fromPureScript y
  Language.PureScript.PrettyPrintFunction x y ->
    throwError (PrettyPrintFunction x y)
  Language.PureScript.PrettyPrintObject x -> throwError (PrettyPrintObject x)
  Language.PureScript.PrettyPrintForAll x y -> throwError (PrettyPrintForAll x y)
  Language.PureScript.BinaryNoParensType (Language.PureScript.TypeOp x') y' z' -> do
    x <- Name.qualified (pure . Name.typeOperator) x'
    y <- fromPureScript y'
    z <- fromPureScript z'
    pure (TypeInfixOperator y x z)
  Language.PureScript.BinaryNoParensType x y z ->
    throwError (InfixTypeNotTypeOp x y z)
  Language.PureScript.ParensInType x -> fmap TypeParens (fromPureScript x)

newtype Variable
  = Variable Text

instance Display Variable where
  display = \case
    Variable x -> "Variable: " <> display x

docFromVariable :: Variable -> Doc a
docFromVariable = \case
  Variable x -> pretty x

newtype Variables a
  = Variables (Maybe (NonEmpty (Variable, Maybe (Kind.Kind a))))
  deriving (Functor)

instance (Display a) => Display (Variables a) where
  display = \case
    Variables Nothing -> "No Variables"
    Variables (Just variables) ->
      "Variables: ["
        <> intercalateMap1 ", " go variables
        <> "]"
        where
        go = \case
          (variable, Nothing) ->
            "Variable: "
              <> display variable
              <> ", No Kind"
          (variable, Just x) ->
            "Variable: "
              <> display variable
              <> ", Kind: "
              <> display x

docFromVariables :: Variables Annotation.Normalized -> Doc b
docFromVariables = \case
  Variables Nothing -> mempty
  Variables (Just variables) -> space <> intercalateMap1 space go variables
    where
    go = \case
      (variable, Nothing) -> docFromVariable variable
      (variable, Just kind') ->
        parens (docFromVariable variable <+> colon <> colon <+> Kind.doc kind')

normalizeVariables :: Variables a -> Variables Annotation.Normalized
normalizeVariables = \case
  Variables variables ->
    Variables ((fmap . fmap . fmap . fmap) Kind.normalize variables)

data Wildcard
  = Wildcard

instance Display Wildcard where
  display = \case
    Wildcard ->
      "Wildcard"

docFromWildcard :: Wildcard -> Doc a
docFromWildcard = \case
  Wildcard -> "_"

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
