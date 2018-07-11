module Declaration.Value where

import "rio" RIO hiding (guard)

import "freer-simple" Control.Monad.Freer        (Eff, Members)
import "freer-simple" Control.Monad.Freer.Error  (Error, throwError)
import "base" Data.List.NonEmpty                 (NonEmpty, nonEmpty)
import "semigroupoids" Data.Semigroup.Foldable   (intercalateMap1)
import "prettyprinter" Data.Text.Prettyprint.Doc
    (pipe, comma,  Doc
    , align
    , colon
    , equals
    , flatAlt
    , group
    , indent
    , lbrace
    , lbracket
    , line
    , parens
    , pretty
    , rbrace
    , rbracket
    , space
    , viaShow
    , vsep
    , (<+>)
    )

import qualified "purescript" Language.PureScript
import qualified "purescript" Language.PureScript.Label
import qualified "purescript" Language.PureScript.PSString

import qualified "this" Annotation
import qualified "this" Comment
import qualified "this" Declaration.Type
import qualified "this" Kind
import qualified "this" Name
import qualified "this" Type
import qualified "this" Variations

data Binder a
  = BinderAs !(Name.Common a) !(Binder a)
  | BinderBinary !(Binder a) !(Name.Qualified Name.ValueOperator a) !(Binder a)
  | BinderCommented !(Binder a) ![Comment.Comment]
  | BinderConstructor
      !(Name.Qualified Name.Constructor a)
      !(Maybe (NonEmpty (Binder a)))
  | BinderLiteral !(Literal (Binder a))
  | BinderOperator !(Name.Qualified Name.ValueOperator a)
  | BinderParens !(Binder a)
  | BinderTyped !(Binder a) !(Type.Type a)
  | BinderVariable !(Name.Common a)
  | BinderWildcard
  deriving (Functor, Show)

binder ::
  ( Members
    '[ Error BinaryBinderWithoutOperator
     , Error NoExpressions
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
  Language.PureScript.Binder ->
  Eff e (Binder Annotation.Unannotated)
binder = \case
  Language.PureScript.BinaryNoParensBinder (Language.PureScript.OpBinder _ x) y z -> do
    left <- binder y
    operator <- Name.qualified (pure . Name.valueOperator) x
    right <- binder z
    pure (BinderBinary left operator right)
  Language.PureScript.BinaryNoParensBinder x y z ->
    throwError (BinaryBinderWithoutOperator y x z)
  Language.PureScript.ConstructorBinder _ x y -> do
    name <- Name.qualified (pure . Name.constructor) x
    binders <- nonEmpty <$> traverse binder y
    pure (BinderConstructor name binders)
  Language.PureScript.LiteralBinder _ x -> fmap BinderLiteral (literal binder x)
  Language.PureScript.NamedBinder _ x y -> do
    name <- Name.common x
    binder' <- binder y
    pure (BinderAs name binder')
  Language.PureScript.NullBinder -> pure BinderWildcard
  Language.PureScript.OpBinder _ x ->
    fmap BinderOperator (Name.qualified (pure . Name.valueOperator) x)
  Language.PureScript.ParensInBinder x -> fmap BinderParens (binder x)
  Language.PureScript.PositionedBinder _ x y -> do
    let comments = fmap Comment.fromPureScript x
    binder' <- binder y
    pure (BinderCommented binder' comments)
  Language.PureScript.TypedBinder x y -> do
    binder' <- binder y
    type' <- Type.fromPureScript x
    pure (BinderTyped binder' type')
  Language.PureScript.VarBinder _ x -> fmap BinderVariable (Name.common x)

docFromBinder :: Binder Annotation.Normalized -> Doc a
docFromBinder = \case
  BinderAs x y -> Name.docFromCommon x <> "@" <> docFromBinder y
  BinderBinary x y z ->
    docFromBinder x
      <+> Name.docFromQualified Name.docFromValueOperator' y
      <+> docFromBinder z
  BinderCommented x y -> foldMap Comment.doc y <> docFromBinder x
  BinderConstructor x Nothing -> Name.docFromQualified Name.docFromConstructor x
  BinderConstructor x (Just y) ->
    Name.docFromQualified Name.docFromConstructor x
      <+> intercalateMap1 space docFromBinder y
  BinderLiteral x ->
    Variations.singleLine (docFromLiteral (pure . docFromBinder) x)
  BinderOperator x -> Name.docFromQualified Name.docFromValueOperator x
  BinderParens x -> parens (docFromBinder x)
  BinderTyped x y ->
    docFromBinder x <+> colon <> colon <+> Variations.singleLine (Type.doc y)
  BinderVariable x -> Name.docFromCommon x
  BinderWildcard -> "_"

labelFromBinder :: Binder a -> Maybe Language.PureScript.Label.Label
labelFromBinder = \case
  BinderAs _ _ -> Nothing
  BinderBinary _ _ _ -> Nothing
  BinderCommented _ _ -> Nothing
  BinderConstructor _ _ -> Nothing
  BinderLiteral _ -> Nothing
  BinderOperator _ -> Nothing
  BinderParens x -> labelFromBinder x
  BinderTyped _ _ -> Nothing
  BinderVariable (Name.Common _ x) -> Just label
    where
    label =
      Language.PureScript.Label.Label (Language.PureScript.PSString.mkString x)
  BinderWildcard -> Nothing

normalizeBinder :: Binder a -> Binder Annotation.Normalized
normalizeBinder = \case
  BinderAs x y -> BinderAs (Annotation.None <$ x) (normalizeBinder y)
  BinderBinary x y z ->
    BinderBinary (normalizeBinder x) (Annotation.None <$ y) (normalizeBinder z)
  BinderCommented x y -> BinderCommented (normalizeBinder x) y
  BinderConstructor x y ->
    BinderConstructor
      (Annotation.None <$ x)
      ((fmap . fmap) normalizeBinder y)
  BinderLiteral x ->
    BinderLiteral (normalizeLiteral labelFromBinder normalizeBinder x)
  BinderOperator x -> BinderOperator (Annotation.None <$ x)
  BinderParens x -> BinderParens (normalizeBinder x)
  BinderTyped x y -> BinderTyped (normalizeBinder x) (Type.normalize y)
  BinderVariable x -> BinderVariable (Annotation.None <$ x)
  BinderWildcard -> BinderWildcard

data CaseAlternative a
  = CaseAlternative !(NonEmpty (Binder a)) !(NonEmpty (GuardedExpression a))
  deriving (Functor, Show)

caseAlternative ::
  ( Members
    '[ Error BinaryBinderWithoutOperator
     , Error CaseAlternativeWithoutBinders
     , Error CaseAlternativeWithoutExpressions
     , Error CaseWithoutAlternatives
     , Error CaseWithoutExpressions
     , Error DoLetWithoutBindings
     , Error DoWithoutStatements
     , Error InvalidExpressions
     , Error InvalidLetBinding
     , Error InvalidWhereDeclaration
     , Error LetWithoutBindings
     , Error NoExpressions
     , Error NotImplemented
     , Error WhereWithoutDeclarations
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
  Language.PureScript.CaseAlternative ->
  Eff e (CaseAlternative Annotation.Unannotated)
caseAlternative = \case
  Language.PureScript.CaseAlternative x y -> do
    binders' <- nonEmpty <$> traverse binder x
    binders <- maybe (throwError CaseAlternativeWithoutBinders) pure binders'
    exprs' <- nonEmpty <$> traverse guardedExpression y
    exprs <- maybe (throwError CaseAlternativeWithoutExpressions) pure exprs'
    pure (CaseAlternative binders exprs)

dynamicCaseAlternative :: CaseAlternative Annotation.Normalized -> Doc a
dynamicCaseAlternative = \case
  CaseAlternative x y ->
    intercalateMap1 (comma <> space) docFromBinder x
      <> align (intercalateMap1 line dynamicGuardedExpression y)

normalizeCaseAlternative ::
  CaseAlternative a ->
  CaseAlternative Annotation.Normalized
normalizeCaseAlternative = \case
  CaseAlternative x y ->
    CaseAlternative (fmap normalizeBinder x) (fmap normalizeGuardedExpression y)

staticCaseAlternative :: CaseAlternative Annotation.Normalized -> Doc a
staticCaseAlternative = \case
  CaseAlternative x y ->
    intercalateMap1 (comma <> space) docFromBinder x
      <> align (intercalateMap1 line staticGuardedExpression y)

data Do a
  = DoBind !(Binder a) !(Expression a)
  | DoCommented ![Comment.Comment] !(Do a)
  | DoExpression !(Expression a)
  | DoLet !(NonEmpty (LetBinding a))
  deriving (Functor, Show)

do' ::
  ( Members
    '[ Error BinaryBinderWithoutOperator
     , Error CaseAlternativeWithoutBinders
     , Error CaseAlternativeWithoutExpressions
     , Error CaseWithoutAlternatives
     , Error CaseWithoutExpressions
     , Error DoLetWithoutBindings
     , Error DoWithoutStatements
     , Error InvalidExpressions
     , Error InvalidLetBinding
     , Error InvalidWhereDeclaration
     , Error LetWithoutBindings
     , Error NoExpressions
     , Error NotImplemented
     , Error WhereWithoutDeclarations
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
  Language.PureScript.DoNotationElement ->
  Eff e (Do Annotation.Unannotated)
do' = \case
  Language.PureScript.DoNotationBind x y -> do
    binder' <- binder x
    expr <- expression y
    pure (DoBind binder' expr)
  Language.PureScript.DoNotationLet x -> do
    bindings' <- nonEmpty <$> traverse letBinding x
    case bindings' of
      Nothing       -> throwError DoLetWithoutBindings
      Just bindings -> pure (DoLet bindings)
  Language.PureScript.DoNotationValue x -> fmap DoExpression (expression x)
  Language.PureScript.PositionedDoNotationElement _ x y -> do
    statement <- do' y
    let comments = fmap Comment.fromPureScript x
    pure (DoCommented comments statement)

dynamicDo :: Do Annotation.Normalized -> Doc a
dynamicDo = \case
  DoBind x y -> docFromBinder x <+> "<-" <+> dynamicExpression y
  DoCommented x y -> foldMap Comment.doc x <> dynamicDo y
  DoExpression x -> dynamicExpression x
  DoLet x -> "let" <+> align (vsep $ toList $ fmap dynamicLetBinding x)

normalizeDo :: Do a -> Do Annotation.Normalized
normalizeDo = \case
  DoBind x y -> DoBind (normalizeBinder x) (normalizeExpression y)
  DoCommented x y -> DoCommented x (normalizeDo y)
  DoExpression x -> DoExpression (normalizeExpression x)
  DoLet x -> DoLet (fmap normalizeLetBinding x)

staticDo :: Do Annotation.Normalized -> Doc a
staticDo = \case
  DoBind x y -> docFromBinder x <+> "<-" <+> staticExpression y
  DoCommented x y -> foldMap Comment.doc x <> staticDo y
  DoExpression x -> staticExpression x
  DoLet x -> "let" <+> align (vsep $ toList $ fmap staticLetBinding x)

data Expression a
  = ExpressionAdo !(NonEmpty (Do a)) !(Expression a)
  | ExpressionApplication !(Expression a) !(Expression a)
  | ExpressionCase !(NonEmpty (Expression a)) !(NonEmpty (CaseAlternative a))
  | ExpressionCommented !(Expression a) ![Comment.Comment]
  | ExpressionConstructor !(Name.Qualified Name.Constructor a)
  | ExpressionDo !(NonEmpty (Do a))
  | ExpressionIf !(Expression a) !(Expression a) !(Expression a)
  | ExpressionInfix !(Expression a) !(Expression a) !(Expression a)
  | ExpressionLet !(NonEmpty (LetBinding a)) !(Expression a)
  | ExpressionLiteral !(Literal (Expression a))
  | ExpressionOperator !(Name.Qualified Name.ValueOperator a)
  | ExpressionParens !(Expression a)
  | ExpressionTyped !(Expression a) !(Type.Type a)
  | ExpressionVariable !(Name.Qualified Name.Common a)
  | ExpressionWhere !(Expression a) !(NonEmpty (WhereDeclaration a))
  deriving (Functor, Show)

dynamicExpression :: Expression Annotation.Normalized -> Doc a
dynamicExpression = \case
  ExpressionAdo x y ->
    "ado"
      <> line
      <> indent 2 (align $ vsep (toList $ fmap dynamicDo x) <> expressionDoc)
      where
      expressionDoc = line <> "in" <+> dynamicExpression y
  ExpressionApplication x y -> dynamicExpression x <+> dynamicExpression y
  ExpressionCase x y ->
    "case" <+> intercalateMap1 (comma <> space) dynamicExpression x <+> "of"
      <> line
      <> indent 2 (align $ intercalateMap1 line dynamicCaseAlternative y)
  ExpressionCommented x y -> foldMap Comment.doc y <> dynamicExpression x
  ExpressionConstructor x -> Name.docFromQualified Name.docFromConstructor x
  ExpressionDo x ->
    "do"
      <> line
      <> indent 2 (align $ vsep $ toList $ fmap dynamicDo x)
  ExpressionIf x y z ->
    "if"
      <+> dynamicExpression x
      <> line
      <> indent 2 (align $ vsep cases)
      where
      cases = ["then" <+> dynamicExpression y, "else" <+> dynamicExpression z]
  ExpressionInfix left (ExpressionOperator op) right ->
    dynamicExpression left
      <+> Name.docFromQualified Name.docFromValueOperator' op
      <+> dynamicExpression right
  ExpressionInfix left (ExpressionCommented op comments) right ->
    dynamicExpression left
      <+> foldMap Comment.doc comments
      <> "`" <> dynamicExpression op <> "`"
      <+> dynamicExpression right
  ExpressionInfix left op right ->
    dynamicExpression left
      <+> "`" <> dynamicExpression op <> "`"
      <+> dynamicExpression right
  ExpressionLet x y ->
    align $ vsep
      [ "let" <+> align (vsep $ toList $ fmap dynamicLetBinding x)
      , "in" <+> dynamicExpression y
      ]
  ExpressionLiteral x -> group (flatAlt multiLine singleLine)
    where
    Variations.Variations { Variations.multiLine, Variations.singleLine } =
      docFromLiteral (pure . dynamicExpression) x
  ExpressionOperator x -> Name.docFromQualified Name.docFromValueOperator x
  ExpressionParens x -> parens (dynamicExpression x)
  ExpressionTyped x y ->
    staticExpression x <+> colon <> colon <+> Variations.singleLine (Type.doc y)
  ExpressionVariable x -> Name.docFromQualified Name.docFromCommon x
  ExpressionWhere x y -> whereDoc
    where
    whereDeclarations = fmap docFromWhereDeclaration y
    whereDoc =
      dynamicExpression x
        <> line
        <> indent 2 (align $ vsep $ "where" : toList whereDeclarations)

expression ::
  ( Members
    '[ Error BinaryBinderWithoutOperator
     , Error CaseAlternativeWithoutBinders
     , Error CaseAlternativeWithoutExpressions
     , Error CaseWithoutAlternatives
     , Error CaseWithoutExpressions
     , Error DoLetWithoutBindings
     , Error DoWithoutStatements
     , Error InvalidExpressions
     , Error InvalidLetBinding
     , Error InvalidWhereDeclaration
     , Error LetWithoutBindings
     , Error NoExpressions
     , Error NotImplemented
     , Error WhereWithoutDeclarations
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
  Language.PureScript.Expr ->
  Eff e (Expression Annotation.Unannotated)
expression = \case
  Language.PureScript.Ado x y -> do
    statements' <- nonEmpty <$> traverse do' x
    expr <- expression y
    case statements' of
      Nothing         -> throwError DoWithoutStatements
      Just statements -> pure (ExpressionAdo statements expr)
  Language.PureScript.App x y ->
    ExpressionApplication <$> expression x <*> expression y
  Language.PureScript.BinaryNoParens x y z -> do
    ExpressionInfix <$> expression y <*> expression x <*> expression z
  Language.PureScript.Case x y -> do
    exprs' <- nonEmpty <$> traverse expression x
    exprs <- maybe (throwError CaseWithoutExpressions) pure exprs'
    alts' <- nonEmpty <$> traverse caseAlternative y
    alts <- maybe (throwError CaseWithoutAlternatives) pure alts'
    pure (ExpressionCase exprs alts)
  Language.PureScript.Constructor _ x ->
    fmap ExpressionConstructor (Name.qualified (pure . Name.constructor) x)
  Language.PureScript.Do x -> do
    statements' <- nonEmpty <$> traverse do' x
    case statements' of
      Nothing         -> throwError DoWithoutStatements
      Just statements -> pure (ExpressionDo statements)
  Language.PureScript.IfThenElse x y z ->
    ExpressionIf <$> expression x <*> expression y <*> expression z
  Language.PureScript.Let Language.PureScript.FromLet x y -> do
    bindings' <- nonEmpty <$> traverse letBinding x
    expr <- expression y
    case bindings' of
      Nothing       -> throwError (LetWithoutBindings y)
      Just bindings -> pure (ExpressionLet bindings expr)
  Language.PureScript.Let Language.PureScript.FromWhere x y -> do
    expr <- expression y
    declarations' <- nonEmpty <$> traverse whereDeclaration x
    case declarations' of
      Nothing           -> throwError (WhereWithoutDeclarations y)
      Just declarations -> pure (ExpressionWhere expr declarations)
  Language.PureScript.Literal _ x ->
    fmap ExpressionLiteral (literal expression x)
  Language.PureScript.Op _ x ->
    fmap ExpressionOperator (Name.qualified (pure . Name.valueOperator) x)
  Language.PureScript.Parens x -> fmap ExpressionParens (expression x)
  Language.PureScript.PositionedValue _ x y -> do
    let comments = fmap Comment.fromPureScript x
    expr <- expression y
    pure (ExpressionCommented expr comments)
  Language.PureScript.TypedValue _ x y -> do
    expr <- expression x
    type' <- Type.fromPureScript y
    pure (ExpressionTyped expr type')
  Language.PureScript.Var _ x -> do
    name <- Name.qualified Name.common x
    pure (ExpressionVariable name)
  x -> throwError (NotImplemented x)

labelFromExpression :: Expression a -> Maybe Language.PureScript.Label.Label
labelFromExpression = \case
  ExpressionAdo _ _ -> Nothing
  ExpressionApplication _ _ -> Nothing
  ExpressionCase _ _ -> Nothing
  ExpressionCommented _ _ -> Nothing
  ExpressionConstructor (Name.Qualified (Just _) _) -> Nothing
  ExpressionConstructor (Name.Qualified Nothing (Name.Constructor (Name.Proper _ x))) ->
    Just label
      where
      label =
        Language.PureScript.Label.Label (Language.PureScript.PSString.mkString x)
  ExpressionDo _ -> Nothing
  ExpressionIf _ _ _ -> Nothing
  ExpressionInfix _ _ _ -> Nothing
  ExpressionLet _ _ -> Nothing
  ExpressionLiteral _ -> Nothing
  ExpressionOperator _ -> Nothing
  ExpressionParens x -> labelFromExpression x
  ExpressionTyped _ _ -> Nothing
  ExpressionVariable (Name.Qualified (Just _) _) -> Nothing
  ExpressionVariable (Name.Qualified Nothing (Name.Common _ x)) -> Just label
    where
    label =
      Language.PureScript.Label.Label (Language.PureScript.PSString.mkString x)
  ExpressionWhere _ _ -> Nothing

normalizeExpression :: Expression a -> Expression Annotation.Normalized
normalizeExpression = \case
  ExpressionAdo x y ->
    ExpressionAdo (fmap normalizeDo x) (normalizeExpression y)
  ExpressionApplication x y ->
    ExpressionApplication (normalizeExpression x) (normalizeExpression y)
  ExpressionCase x y ->
    ExpressionCase (fmap normalizeExpression x) (fmap normalizeCaseAlternative y)
  ExpressionCommented x y -> ExpressionCommented (normalizeExpression x) y
  ExpressionConstructor x -> ExpressionConstructor (Annotation.None <$ x)
  ExpressionDo x -> ExpressionDo (fmap normalizeDo x)
  ExpressionIf x y z ->
    ExpressionIf
      (normalizeExpression x)
      (normalizeExpression y)
      (normalizeExpression z)
  ExpressionInfix x y z ->
    ExpressionInfix
      (normalizeExpression x)
      (normalizeExpression y)
      (normalizeExpression z)
  ExpressionLet x y ->
    ExpressionLet (fmap normalizeLetBinding x) (normalizeExpression y)
  ExpressionLiteral x ->
    ExpressionLiteral
      (normalizeLiteral labelFromExpression normalizeExpression x)
  ExpressionOperator x -> ExpressionOperator (Annotation.None <$ x)
  ExpressionParens x -> ExpressionParens (normalizeExpression x)
  ExpressionTyped x y ->
    ExpressionTyped (normalizeExpression x) (Type.normalize y)
  ExpressionVariable x -> ExpressionVariable (Annotation.None <$ x)
  ExpressionWhere x y ->
    ExpressionWhere (normalizeExpression x) (fmap normalizeWhereDeclaration y)

staticExpression :: Expression Annotation.Normalized -> Doc a
staticExpression = \case
  ExpressionAdo x y ->
    "ado"
      <> line
      <> indent 2 (align $ vsep (toList $ fmap staticDo x) <> expressionDoc)
      where
      expressionDoc = line <> "in" <+> staticExpression y
  ExpressionApplication x y -> staticExpression x <+> staticExpression y
  ExpressionCase x y ->
    "case" <+> intercalateMap1 (comma <> space) staticExpression x <+> "of"
      <> line
      <> indent 2 (align $ intercalateMap1 line staticCaseAlternative y)
  ExpressionCommented x y -> foldMap Comment.doc y <> staticExpression x
  ExpressionConstructor x -> Name.docFromQualified Name.docFromConstructor x
  ExpressionDo x ->
    "do"
      <> line
      <> indent 2 (align $ vsep $ toList $ fmap staticDo x)
  ExpressionIf x y z ->
    "if"
      <+> staticExpression x
      <> line
      <> indent 2 (align $ vsep cases)
      where
      cases = ["then" <+> staticExpression y, "else" <+> staticExpression z]
  ExpressionInfix left (ExpressionOperator op) right ->
    staticExpression left
      <+> Name.docFromQualified Name.docFromValueOperator' op
      <+> staticExpression right
  ExpressionInfix left (ExpressionCommented op comments) right ->
    staticExpression left
      <+> foldMap Comment.doc comments
      <> "`" <> staticExpression op <> "`"
      <+> staticExpression right
  ExpressionInfix left op right ->
    staticExpression left
      <+> "`" <> staticExpression op <> "`"
      <+> staticExpression right
  ExpressionLet x y ->
    align $ vsep
      [ "let" <+> align (vsep $ toList $ fmap staticLetBinding x)
      , "in" <+> staticExpression y
      ]
  ExpressionLiteral x ->
    Variations.multiLine (docFromLiteral (pure . staticExpression) x)
  ExpressionOperator x -> Name.docFromQualified Name.docFromValueOperator x
  ExpressionParens x -> parens (staticExpression x)
  ExpressionTyped x y ->
    staticExpression x <+> colon <> colon <+> Variations.multiLine (Type.doc y)
  ExpressionVariable x -> Name.docFromQualified Name.docFromCommon x
  ExpressionWhere x y -> whereDoc
    where
    whereDeclarations = fmap docFromWhereDeclaration y
    whereDoc =
      staticExpression x
        <> line
        <> indent 2 (align $ vsep $ "where" : toList whereDeclarations)

data Guard a
  = GuardBinder !(Binder a) !(Expression a)
  | GuardExpression !(Expression a)
  deriving (Functor, Show)

dynamicGuard :: Guard Annotation.Normalized -> Doc a
dynamicGuard = \case
  GuardBinder x y -> docFromBinder x <+> "<-" <+> dynamicExpression y
  GuardExpression x -> dynamicExpression x

guard ::
  ( Members
    '[ Error BinaryBinderWithoutOperator
     , Error CaseAlternativeWithoutBinders
     , Error CaseAlternativeWithoutExpressions
     , Error CaseWithoutAlternatives
     , Error CaseWithoutExpressions
     , Error DoLetWithoutBindings
     , Error DoWithoutStatements
     , Error InvalidExpressions
     , Error InvalidLetBinding
     , Error InvalidWhereDeclaration
     , Error LetWithoutBindings
     , Error NoExpressions
     , Error NotImplemented
     , Error WhereWithoutDeclarations
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
  Language.PureScript.Guard ->
  Eff e (Guard Annotation.Unannotated)
guard = \case
  Language.PureScript.ConditionGuard x -> fmap GuardExpression (expression x)
  Language.PureScript.PatternGuard x y ->
    GuardBinder <$> (binder x) <*> (expression y)

normalizeGuard :: Guard a -> Guard Annotation.Normalized
normalizeGuard = \case
  GuardBinder x y -> GuardBinder (normalizeBinder x) (normalizeExpression y)
  GuardExpression x -> GuardExpression (normalizeExpression x)

staticGuard :: Guard Annotation.Normalized -> Doc a
staticGuard = \case
  GuardBinder x y -> docFromBinder x <+> "<-" <+> staticExpression y
  GuardExpression x -> staticExpression x

data GuardedExpression a
  = GuardedExpression !(Maybe (NonEmpty (Guard a))) !(Expression a)
  deriving (Functor, Show)

dynamicGuardedExpression :: GuardedExpression Annotation.Normalized -> Doc a
dynamicGuardedExpression = \case
  GuardedExpression (Just x) y ->
    space
      <> pipe
      <+> intercalateMap1 (comma <> space) dynamicGuard x
      <+> "->"
      <+> dynamicExpression y
  GuardedExpression Nothing y -> space <> "->" <+> dynamicExpression y

guardedExpression ::
  ( Members
    '[ Error BinaryBinderWithoutOperator
     , Error CaseAlternativeWithoutBinders
     , Error CaseAlternativeWithoutExpressions
     , Error CaseWithoutAlternatives
     , Error CaseWithoutExpressions
     , Error DoLetWithoutBindings
     , Error DoWithoutStatements
     , Error InvalidExpressions
     , Error InvalidLetBinding
     , Error InvalidWhereDeclaration
     , Error LetWithoutBindings
     , Error NoExpressions
     , Error NotImplemented
     , Error WhereWithoutDeclarations
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
  Language.PureScript.GuardedExpr ->
  Eff e (GuardedExpression Annotation.Unannotated)
guardedExpression = \case
  Language.PureScript.GuardedExpr x y -> do
    guards <- nonEmpty <$> traverse guard x
    expr <- expression y
    pure (GuardedExpression guards expr)

normalizeGuardedExpression ::
  GuardedExpression a ->
  GuardedExpression Annotation.Normalized
normalizeGuardedExpression = \case
  GuardedExpression x y ->
    GuardedExpression ((fmap . fmap) normalizeGuard x) (normalizeExpression y)

staticGuardedExpression :: GuardedExpression Annotation.Normalized -> Doc a
staticGuardedExpression = \case
  GuardedExpression (Just x) y ->
    space
      <> pipe
      <+> intercalateMap1 (comma <> space) staticGuard x
      <+> "->"
      <+> staticExpression y
  GuardedExpression Nothing y -> space <> "->" <+> staticExpression y

data LetBinding a
  = LetBindingType !(Declaration.Type.Type a)
  | LetBindingValue !(Value a)
  deriving (Functor, Show)

dynamicLetBinding :: LetBinding Annotation.Normalized -> Doc a
dynamicLetBinding = \case
  LetBindingType x -> group (flatAlt multiLine singleLine)
    where
    Variations.Variations { Variations.multiLine, Variations.singleLine } =
      Declaration.Type.doc x
  LetBindingValue x -> static x

normalizeLetBinding ::
  LetBinding a ->
  LetBinding Annotation.Normalized
normalizeLetBinding = \case
  LetBindingType x -> LetBindingType (Declaration.Type.normalize x)
  LetBindingValue x -> LetBindingValue (normalize x)

letBinding ::
  ( Members
    '[ Error BinaryBinderWithoutOperator
     , Error CaseAlternativeWithoutBinders
     , Error CaseAlternativeWithoutExpressions
     , Error CaseWithoutAlternatives
     , Error CaseWithoutExpressions
     , Error DoLetWithoutBindings
     , Error DoWithoutStatements
     , Error InvalidExpressions
     , Error InvalidLetBinding
     , Error InvalidWhereDeclaration
     , Error LetWithoutBindings
     , Error NoExpressions
     , Error NotImplemented
     , Error WhereWithoutDeclarations
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
  Eff e (LetBinding Annotation.Unannotated)
letBinding = \case
  Language.PureScript.TypeDeclaration x ->
    fmap LetBindingType (Declaration.Type.fromPureScript x)
  Language.PureScript.ValueDeclaration x ->
    fmap LetBindingValue (fromPureScript x)
  x -> throwError (InvalidLetBinding x)

staticLetBinding :: LetBinding Annotation.Normalized -> Doc a
staticLetBinding = \case
  LetBindingType x -> Variations.multiLine (Declaration.Type.doc x)
  LetBindingValue x -> static x

data Literal a
  = LiteralArray !(Maybe (NonEmpty a))
  | LiteralBoolean !Bool
  | LiteralChar !Char
  | LiteralInt !Integer
  | LiteralNumber !Double
  | LiteralRecord !(Maybe (NonEmpty (RecordPair a)))
  | LiteralString !Language.PureScript.PSString.PSString
  deriving (Functor, Show)

docFromLiteral ::
  (a -> Variations.Variations (Doc b)) ->
  Literal a ->
  Variations.Variations (Doc b)
docFromLiteral f = \case
  LiteralArray Nothing -> pure (lbracket <> rbracket)
  LiteralArray (Just x) -> Variations.bracketesize f x
  LiteralBoolean True -> pure "true"
  LiteralBoolean False -> pure "false"
  LiteralChar x -> pure (viaShow x)
  LiteralInt x -> pure (pretty x)
  LiteralNumber x -> pure (pretty x)
  LiteralRecord Nothing -> pure (lbrace <> rbrace)
  LiteralRecord (Just x) -> Variations.bracesize (docFromRecordPair f) x
  LiteralString x -> pure (pretty $ Language.PureScript.prettyPrintString x)

literal :: (a -> Eff e b) -> Language.PureScript.Literal a -> Eff e (Literal b)
literal f = \case
  Language.PureScript.ArrayLiteral x -> LiteralArray . nonEmpty <$> traverse f x
  Language.PureScript.BooleanLiteral x -> pure (LiteralBoolean x)
  Language.PureScript.CharLiteral x -> pure (LiteralChar x)
  Language.PureScript.NumericLiteral (Left x) -> pure (LiteralInt x)
  Language.PureScript.NumericLiteral (Right x) -> pure (LiteralNumber x)
  Language.PureScript.ObjectLiteral x ->
    LiteralRecord . nonEmpty <$> traverse (recordPair f) x
  Language.PureScript.StringLiteral x -> pure (LiteralString x)

normalizeLiteral ::
  (f a -> Maybe Language.PureScript.Label.Label) ->
  (f a -> f Annotation.Normalized) ->
  Literal (f a) ->
  Literal (f Annotation.Normalized)
normalizeLiteral f g = \case
  LiteralArray x -> LiteralArray ((fmap . fmap) g x)
  LiteralBoolean x -> LiteralBoolean x
  LiteralChar x -> LiteralChar x
  LiteralInt x -> LiteralInt x
  LiteralNumber x -> LiteralNumber x
  LiteralRecord x -> LiteralRecord ((fmap . fmap) (normalizeRecordPair f g) x)
  LiteralString x -> LiteralString x

data RecordPair a
  = RecordPair !Language.PureScript.Label.Label !a
  | RecordPun !Language.PureScript.Label.Label
  deriving (Functor, Show)

docFromRecordPair ::
  (a -> Variations.Variations (Doc b)) ->
  RecordPair a ->
  Variations.Variations (Doc b)
docFromRecordPair f = \case
  RecordPair x y ->
    Variations.Variations { Variations.multiLine, Variations.singleLine }
      where
      multiLine =
        pretty (Language.PureScript.prettyPrintLabel x)
          <> colon
          <+> Variations.multiLine (f y)
      singleLine =
        pretty (Language.PureScript.prettyPrintLabel x)
          <> colon
          <+> Variations.singleLine (f y)
  RecordPun x -> pure (pretty $ Language.PureScript.prettyPrintLabel x)

normalizeRecordPair ::
  (f a -> Maybe Language.PureScript.Label.Label) ->
  (f a -> f Annotation.Normalized) ->
  RecordPair (f a) ->
  RecordPair (f Annotation.Normalized)
normalizeRecordPair f g = \case
  RecordPair x y -> case f y of
    Nothing -> RecordPair x (g y)
    Just label
      | label == x -> RecordPun x
      | otherwise -> RecordPair x (g y)
  RecordPun x -> RecordPun x

recordPair ::
  (a -> Eff e b) ->
  (Language.PureScript.PSString.PSString, a) ->
  Eff e (RecordPair b)
recordPair f = \case
  (x, y) -> fmap (RecordPair $ Language.PureScript.Label.Label x) (f y)

data Value a
  = ValueExpression
      !(Name.Common a)
      !(Maybe (NonEmpty (Binder a)))
      !(Expression a)
  deriving (Functor, Show)

dynamic, static :: Value Annotation.Normalized -> Doc a
(dynamic, static) = (dynamic', static')
  where
  bindersDoc binders = space <> intercalateMap1 space docFromBinder binders
  dynamic' = \case
    ValueExpression x y z -> doc
      where
      doc =
        Name.docFromCommon x
          <> foldMap bindersDoc y
          <+> equals
          <+> dynamicExpression z
  static' = \case
    ValueExpression x y z -> doc
      where
      doc =
        Name.docFromCommon x
          <> foldMap bindersDoc y
          <+> equals
          <+> staticExpression z

fromPureScript ::
  ( Members
    '[ Error BinaryBinderWithoutOperator
     , Error CaseAlternativeWithoutBinders
     , Error CaseAlternativeWithoutExpressions
     , Error CaseWithoutAlternatives
     , Error CaseWithoutExpressions
     , Error DoLetWithoutBindings
     , Error DoWithoutStatements
     , Error InvalidExpressions
     , Error InvalidLetBinding
     , Error InvalidWhereDeclaration
     , Error LetWithoutBindings
     , Error NoExpressions
     , Error NotImplemented
     , Error WhereWithoutDeclarations
     , Error Name.InvalidCommon
     , Error Name.Missing
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
  Language.PureScript.ValueDeclarationData [Language.PureScript.GuardedExpr] ->
  Eff e (Value Annotation.Unannotated)
fromPureScript = \case
  Language.PureScript.ValueDeclarationData _ name _ _ [] ->
    throwError (NoExpressions name)
  Language.PureScript.ValueDeclarationData _ name' _ binders' [Language.PureScript.GuardedExpr [] expr'] -> do
    name <- Name.common name'
    binders <- nonEmpty <$> traverse binder binders'
    expr <- expression expr'
    pure (ValueExpression name binders expr)
  Language.PureScript.ValueDeclarationData _ name _ _ exprs ->
    throwError (InvalidExpressions name exprs)

normalize :: Value a -> Value Annotation.Normalized
normalize = \case
  ValueExpression name binders expr ->
    ValueExpression
      (Annotation.None <$ name)
      ((fmap . fmap) normalizeBinder binders)
      (normalizeExpression expr)

data WhereDeclaration a
  = WhereDeclarationType !(Declaration.Type.Type a)
  | WhereDeclarationValue !(Value a)
  deriving (Functor, Show)

docFromWhereDeclaration :: WhereDeclaration Annotation.Normalized -> Doc a
docFromWhereDeclaration = \case
  WhereDeclarationType x -> Variations.multiLine (Declaration.Type.doc x)
  WhereDeclarationValue x -> static x

normalizeWhereDeclaration ::
  WhereDeclaration a ->
  WhereDeclaration Annotation.Normalized
normalizeWhereDeclaration = \case
  WhereDeclarationType x -> WhereDeclarationType (Declaration.Type.normalize x)
  WhereDeclarationValue x -> WhereDeclarationValue (normalize x)

whereDeclaration ::
  ( Members
    '[ Error BinaryBinderWithoutOperator
     , Error CaseAlternativeWithoutBinders
     , Error CaseAlternativeWithoutExpressions
     , Error CaseWithoutAlternatives
     , Error CaseWithoutExpressions
     , Error DoLetWithoutBindings
     , Error DoWithoutStatements
     , Error InvalidExpressions
     , Error InvalidLetBinding
     , Error InvalidWhereDeclaration
     , Error LetWithoutBindings
     , Error NoExpressions
     , Error NotImplemented
     , Error WhereWithoutDeclarations
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
  Eff e (WhereDeclaration Annotation.Unannotated)
whereDeclaration = \case
  Language.PureScript.TypeDeclaration x ->
    fmap WhereDeclarationType (Declaration.Type.fromPureScript x)
  Language.PureScript.ValueDeclaration x ->
    fmap WhereDeclarationValue (fromPureScript x)
  x -> throwError (InvalidWhereDeclaration x)

-- Errors

type Errors
  = '[ Error BinaryBinderWithoutOperator
     , Error CaseAlternativeWithoutBinders
     , Error CaseAlternativeWithoutExpressions
     , Error CaseWithoutAlternatives
     , Error CaseWithoutExpressions
     , Error DoLetWithoutBindings
     , Error DoWithoutStatements
     , Error InvalidExpressions
     , Error InvalidLetBinding
     , Error InvalidWhereDeclaration
     , Error LetWithoutBindings
     , Error NoExpressions
     , Error NotImplemented
     , Error WhereWithoutDeclarations
     ]

data BinaryBinderWithoutOperator
  = BinaryBinderWithoutOperator
      !Language.PureScript.Binder
      !Language.PureScript.Binder
      !Language.PureScript.Binder

instance Display BinaryBinderWithoutOperator where
  display = \case
    BinaryBinderWithoutOperator x y z ->
      "We received a binary binder with `"
        <> displayShow y
        <> "` as the operator."
        <> " The left side was `"
        <> displayShow x
        <> "`. The right side was `"
        <> displayShow z
        <> "`."
        <> " If there is an operator nested within, we should handle that case."
        <> " Otherwise, this is probably a problem in the PureScript library."

data CaseAlternativeWithoutBinders
  = CaseAlternativeWithoutBinders

instance Display CaseAlternativeWithoutBinders where
  display = \case
    CaseAlternativeWithoutBinders ->
      "We received a case alternative that did not have any binders."
        <> " This is probably a problem in the PureScript library."

data CaseAlternativeWithoutExpressions
  = CaseAlternativeWithoutExpressions

instance Display CaseAlternativeWithoutExpressions where
  display = \case
    CaseAlternativeWithoutExpressions ->
      "We received a case alternative that did not have any expressions."
        <> " This is probably a problem in the PureScript library."

data CaseWithoutAlternatives
  = CaseWithoutAlternatives

instance Display CaseWithoutAlternatives where
  display = \case
    CaseWithoutAlternatives ->
      "We received a case that did not have any case alternatives."
        <> " This is probably a problem in the PureScript library."

data CaseWithoutExpressions
  = CaseWithoutExpressions

instance Display CaseWithoutExpressions where
  display = \case
    CaseWithoutExpressions ->
      "We received a case that did not have any case expressions."
        <> " This is probably a problem in the PureScript library."

data DoLetWithoutBindings
  = DoLetWithoutBindings

instance Display DoLetWithoutBindings where
  display = \case
    DoLetWithoutBindings ->
      "We received a let binding in a do expression without any bindings."
        <> " This is probably a problem in the PureScript library."

data DoWithoutStatements
  = DoWithoutStatements

instance Display DoWithoutStatements where
  display = \case
    DoWithoutStatements ->
      "We received a do expression without any statements."
        <> " This is probably a problem in the PureScript library."

data GuardedExpressionWithoutGuards
  = GuardedExpressionWithoutGuards

instance Display GuardedExpressionWithoutGuards where
  display = \case
    GuardedExpressionWithoutGuards ->
      "We received a guarded expression that did not have any guards."
        <> " This is probably a problem in the PureScript library."

data InvalidExpressions
  = InvalidExpressions
      !Language.PureScript.Ident
      ![Language.PureScript.GuardedExpr]

instance Display InvalidExpressions where
  display = \case
    InvalidExpressions x y ->
      "We received a value `"
        <> displayShow x
        <> "` with the wrong combinations of expressions `"
        <> displayShow y
        <> "`. There should either be exactly one expression without any guards"
        <> ", or at least one expression where all are guarded."

newtype InvalidLetBinding
  = InvalidLetBinding Language.PureScript.Declaration

instance Display InvalidLetBinding where
  display = \case
    InvalidLetBinding x ->
      "We received a binding `"
        <> displayShow x
        <> "` in a let expression."
        <> " But, there should only be type and value bindings."
        <> " If there is a type or value declation within,"
        <> " we should handle this case."
        <> " Otherwise, this is probably a problem in the PureScript lirbary."

newtype InvalidWhereDeclaration
  = InvalidWhereDeclaration Language.PureScript.Declaration

instance Display InvalidWhereDeclaration where
  display = \case
    InvalidWhereDeclaration x ->
      "We received a declaration `"
        <> displayShow x
        <> "` in a where clause."
        <> " But, there should only be type and value declarations."
        <> " If there is a type or value declation within,"
        <> " we should handle this case."
        <> " Otherwise, this is probably a problem in the PureScript lirbary."

newtype LetWithoutBindings
  = LetWithoutBindings Language.PureScript.Expr

instance Display LetWithoutBindings where
  display = \case
    LetWithoutBindings x ->
      "We received a let binding for the expression `"
        <> displayShow x
        <> "`, but it did not have any bindings."

newtype NoExpressions
  = NoExpressions Language.PureScript.Ident

instance Display NoExpressions where
  display = \case
    NoExpressions x ->
      "We recieved a value `"
        <> displayShow x
        <> "` that had no expressions."
        <> " This is most likely a problem with the PureScript library."

newtype NotImplemented
  = NotImplemented Language.PureScript.Expr

instance Display NotImplemented where
  display = \case
    NotImplemented x ->
      "We haven't implemented this type of expression yet `"
        <> displayShow x
        <> "`."

newtype WhereWithoutDeclarations
  = WhereWithoutDeclarations Language.PureScript.Expr

instance Display WhereWithoutDeclarations where
  display = \case
    WhereWithoutDeclarations x ->
      "We received a where clause for the expression `"
        <> displayShow x
        <> "`, but it did not have any declarations."
