module Declaration.Class where

import "rio" RIO

import "freer-simple" Control.Monad.Freer        (Eff, Members)
import "freer-simple" Control.Monad.Freer.Error  (Error, throwError)
import "base" Data.List.NonEmpty
    ( NonEmpty((:|))
    , nonEmpty
    , zipWith
    )
import "semigroupoids" Data.Semigroup.Foldable   (intercalateMap1)
import "prettyprinter" Data.Text.Prettyprint.Doc
    ( Doc
    , comma
    , hardline
    , indent
    , pipe
    , space
    , (<+>)
    )

import qualified "base" Data.List.NonEmpty
import qualified "purescript" Language.PureScript
import qualified "rio" RIO.HashMap

import qualified "this" Annotation
import qualified "this" Declaration.Type
import qualified "this" Kind
import qualified "this" Name
import qualified "this" Type
import qualified "this" Variations

data Class a
  = Class
      !(Maybe (NonEmpty (Type.Constraint a)))
      !(Name.Class a)
      !(Type.Variables a)
      !(Maybe (NonEmpty FunctionalDependency))
      !(Maybe (NonEmpty (Declaration.Type.Type a)))
  deriving (Functor, Show)

doc :: Class Annotation.Normalized -> Variations.Variations (Doc a)
doc = \case
  Class constraints' name' variables' funDeps' methods' ->
    Variations.Variations { Variations.multiLine, Variations.singleLine }
      where
      multiLine =
        "class"
          <> foldMap (Variations.multiLine . constraintsDoc) constraints'
          <+> Name.docFromClass name'
          <> Type.docFromVariables variables'
          <> foldMap functionalDependenciesDoc funDeps'
          <> foldMap (Variations.multiLine . methodsDoc) methods'
          <> hardline
      singleLine =
        "class"
          <> foldMap (Variations.singleLine . constraintsDoc) constraints'
          <+> Name.docFromClass name'
          <> Type.docFromVariables variables'
          <> foldMap functionalDependenciesDoc funDeps'
          <> foldMap (Variations.singleLine . methodsDoc) methods'
          <> hardline
      constraintsDoc constraints =
        fmap
          (\x -> space <> x <+> "=>")
          (Variations.parenthesize Type.docFromConstraint constraints)
      functionalDependenciesDoc funDeps =
        space
          <> pipe
          <> intercalateMap1
            (space <> comma <> space) docFromFunctionalDependency
            funDeps
      methodsDoc methods =
        fmap
          (\x -> space <> "where" <> hardline <> indent 2 x)
          (intercalateMap1 (pure hardline) Declaration.Type.doc methods)

fromPureScript ::
  ( Members
    '[ Error InvalidTypeClassMethod
     , Error MissingTypeVariable
     , Error Kind.InferredKind
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
  [Language.PureScript.Constraint] ->
  Language.PureScript.ProperName 'Language.PureScript.ClassName ->
  [(Text, Maybe Language.PureScript.Kind)] ->
  [Language.PureScript.FunctionalDependency] ->
  [Language.PureScript.Declaration] ->
  Eff e (Class Annotation.Unannotated)
fromPureScript constraints' name' variables' funDeps' methods' = do
  constraints <- nonEmpty <$> traverse Type.constraint constraints'
  let name = Name.class' name'
  variables <- Type.variables variables'
  funDeps <- nonEmpty <$> traverse (functionalDependency variables) funDeps'
  methods <- nonEmpty <$> traverse type' methods'
  pure (Class constraints name variables funDeps methods)

normalize :: Class a -> Class Annotation.Normalized
normalize = \case
  Class constraints name variables funDeps methods ->
    Class
      ((fmap . fmap) Type.normalizeConstraint constraints)
      (Annotation.None <$ name)
      (Type.normalizeVariables variables)
      funDeps
      ((fmap . fmap) Declaration.Type.normalize methods)

data FunctionalDependency
  = FunctionalDependency
      !(Maybe (NonEmpty Type.Variable))
      !(Maybe (NonEmpty Type.Variable))
  deriving (Show)

docFromFunctionalDependency :: FunctionalDependency -> Doc a
docFromFunctionalDependency = \case
  FunctionalDependency x' y' ->
    foldMap (\x -> intercalateMap1 space Type.docFromVariable x <> space) x'
      <> "->"
      <> foldMap (\y -> space <> intercalateMap1 space Type.docFromVariable y) y'

functionalDependency ::
  ( Members
    '[ Error MissingTypeVariable
     ]
    e
  ) =>
  Type.Variables a ->
  Language.PureScript.FunctionalDependency ->
  Eff e FunctionalDependency
functionalDependency variables' = \case
  Language.PureScript.FunctionalDependency x y -> do
    determiners <- nonEmpty <$> traverse findVariable x
    determined <- nonEmpty <$> traverse findVariable y
    pure (FunctionalDependency determiners determined)
  where
  findVariable x =
    maybe
      (throwError $ MissingTypeVariable x)
      pure
      (RIO.HashMap.lookup x variables)
  index = zipWith (\x (y, _) -> (x, y)) (0 :| [1..])
  variables = case variables' of
    Type.Variables x ->
      foldMap (RIO.HashMap.fromList . Data.List.NonEmpty.toList . index) x

type' ::
  ( Members
    '[ Error InvalidTypeClassMethod
     , Error Kind.InferredKind
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
  Language.PureScript.Declaration ->
  Eff e (Declaration.Type.Type Annotation.Unannotated)
type' = \case
  Language.PureScript.TypeDeclaration x -> Declaration.Type.fromPureScript x
  x -> throwError (InvalidTypeClassMethod x)

-- Errors

type Errors
  = '[ Error InvalidTypeClassMethod
     , Error MissingTypeVariable
     ]

newtype InvalidTypeClassMethod
  = InvalidTypeClassMethod Language.PureScript.Declaration

instance Display InvalidTypeClassMethod where
  display = \case
    InvalidTypeClassMethod x ->
      "Type class methods can only be type declarations."
        <> " However, we received a `"
        <> displayShow x
        <> "`. This is probably a problem with the PureScript library."

newtype MissingTypeVariable
  = MissingTypeVariable Int

instance Display MissingTypeVariable where
  display = \case
    MissingTypeVariable x ->
      "The type variable at index `"
        <> displayShow x
        <> "` does not exist."
        <> " The functional dependencies are desugared to indicies by the"
        <> " PureScript library."
        <> " Either we're off on our interpretation of those indicies,"
        <> " or there's a problem in the PureScript library."
