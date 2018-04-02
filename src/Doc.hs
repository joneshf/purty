module Doc where

import "protolude" Protolude hiding (group)

import "containers" Data.IntMap.Strict           (findWithDefault, fromList)
import "base" Data.List                          (span)
import "prettyprinter" Data.Text.Prettyprint.Doc
    ( Doc
    , align
    , cat
    , comma
    , enclose
    , encloseSep
    , flatAlt
    , group
    , hcat
    , hsep
    , indent
    , line
    , parens
    , pretty
    , punctuate
    , sep
    , space
    , vsep
    , (<+>)
    )
import "purescript" Language.PureScript
    ( Binder
    , CaseAlternative(CaseAlternative)
    , Comment(BlockComment, LineComment)
    , Constraint(Constraint)
    , DataDeclType(Data, Newtype)
    , Declaration(BindingGroupDeclaration, BoundValueDeclaration, DataBindingGroupDeclaration, DataDeclaration, ExternDataDeclaration, ExternDeclaration, ExternKindDeclaration, FixityDeclaration, ImportDeclaration, TypeClassDeclaration, TypeDeclaration, TypeInstanceDeclaration, TypeSynonymDeclaration, ValueDeclaration)
    , DeclarationRef(KindRef, ModuleRef, ReExportRef, TypeClassRef, TypeInstanceRef, TypeOpRef, TypeRef, ValueOpRef, ValueRef)
    , DoNotationElement(DoNotationBind, DoNotationLet, DoNotationValue, PositionedDoNotationElement)
    , Expr(Abs, Accessor, AnonymousArgument, App, BinaryNoParens, Case, Constructor, DeferredDictionary, Do, Hole, IfThenElse, Let, Literal, ObjectUpdate, ObjectUpdateNested, Op, Parens, PositionedValue, TypeClassDictionary, TypeClassDictionaryAccessor, TypeClassDictionaryConstructorApp, TypedValue, UnaryMinus, Var)
    , Fixity(Fixity)
    , FunctionalDependency(FunctionalDependency)
    , Guard(ConditionGuard, PatternGuard)
    , GuardedExpr(GuardedExpr)
    , Ident
    , ImportDeclarationType(Explicit, Hiding, Implicit)
    , Kind
    , Literal(ArrayLiteral, BooleanLiteral, CharLiteral, NumericLiteral, ObjectLiteral, StringLiteral)
    , Module(Module)
    , ModuleName
    , NameKind
    , PathNode(Branch, Leaf)
    , PathTree(PathTree)
    , ProperName
    , ProperNameType(ClassName, ConstructorName)
    , SourceAnn
    , Type(BinaryNoParensType, ConstrainedType, ForAll, KindedType, ParensInType, PrettyPrintForAll, PrettyPrintFunction, PrettyPrintObject, RCons, REmpty, Skolem, TUnknown, TypeApp, TypeConstructor, TypeLevelString, TypeOp, TypeVar, TypeWildcard)
    , TypeDeclarationData(TypeDeclarationData)
    , TypeFixity(TypeFixity)
    , TypeInstanceBody(DerivedInstance, ExplicitInstance, NewtypeInstance, NewtypeInstanceWithDictionary)
    , ValueDeclarationData(ValueDeclarationData)
    , ValueFixity(ValueFixity)
    , caseAlternativeBinders
    , caseAlternativeResult
    , constraintArgs
    , constraintClass
    , everywhereOnTypes
    , everywhereOnTypesTopDown
    , fdDetermined
    , fdDeterminers
    , isImportDecl
    , prettyPrintBinder
    , prettyPrintKind
    , prettyPrintString
    , prettyPrintType
    , runAssocList
    , runIdent
    , runModuleName
    , runOpName
    , runProperName
    , showAssoc
    , showOp
    , showQualified
    , tyFunction
    , tyRecord
    , tydeclIdent
    , tydeclType
    , valdeclBinders
    , valdeclExpression
    , valdeclIdent
    , valdeclName
    , valdeclSourceAnn
    )
import "purescript" Language.PureScript.Label    (runLabel)
import "purescript" Language.PureScript.PSString (PSString)

braces :: [Doc a] -> Doc a
braces = enclosedWith "{" "}"

brackets :: [Doc a] -> Doc a
brackets = enclosedWith "[" "]"

enclosedWith :: Doc a -> Doc a -> [Doc a] -> Doc a
enclosedWith open close =
  align
    . group
    . encloseSep
      (flatAlt (open <> space) open)
      (flatAlt (line <> close) close)
      ", "

fromBinder :: Binder -> Doc a
fromBinder = pretty . prettyPrintBinder

fromBinders :: [Binder] -> Doc a
fromBinders = \case
  [] -> mempty
  binders -> space <> hsep (map fromBinder binders)

fromCaseAlternative :: CaseAlternative -> Doc a
fromCaseAlternative CaseAlternative {caseAlternativeBinders, caseAlternativeResult} =
  hsep (punctuate comma $ map fromBinder caseAlternativeBinders)
    <+> foldMap fromGuardedExprCase caseAlternativeResult

fromComment :: Comment -> Doc a
fromComment = \case
  BlockComment comment -> enclose "{-" "-}" (pretty comment) <> line
  LineComment comment -> "--" <> pretty comment <> line

fromConstraint :: Language.PureScript.Constraint -> Doc a
fromConstraint Constraint { constraintArgs, constraintClass } =
  pretty (showQualified runProperName constraintClass)
    <+> foldMap fromType constraintArgs

fromConstructor :: ProperName 'ConstructorName -> Doc a
fromConstructor = pretty . runProperName

fromConstructors :: [ProperName 'ConstructorName] -> Doc a
fromConstructors [] = mempty
fromConstructors constructors =
  parentheses $ map fromConstructor constructors

fromDataConstructor :: (ProperName 'ConstructorName, [Language.PureScript.Type]) -> Doc a
fromDataConstructor (name, types) =
  pretty (runProperName name) <+> sep (map (pretty . prettyPrintType) types)

fromDataConstructors :: [(ProperName 'ConstructorName, [Language.PureScript.Type])] -> Doc a
fromDataConstructors =
  vsep . zipWith (<+>) ("=" : repeat "|") . map fromDataConstructor

fromDataType :: DataDeclType -> Doc a
fromDataType = \case
  Data -> "data"
  Newtype -> "newtype"

fromDeclaration :: Declaration -> Doc a
fromDeclaration = \case
  BindingGroupDeclaration declarations ->
    vsep (toList $ map (fromDeclaration . ValueDeclaration . valueDeclarationFromAnonymousDeclaration) declarations)
  BoundValueDeclaration _ binder expr ->
    fromBinder binder <+> "=" <+> fromExpr expr
  DataBindingGroupDeclaration (declarations) ->
    vsep (toList $ map fromDeclaration declarations)
  DataDeclaration _ dataType name parameters constructors ->
    fromDataType dataType
      <+> pretty (runProperName name)
      <+> fromParameters parameters
      <> line
      <> indent 2 (fromDataConstructors constructors)
      <> line
      <> line
  ExternDataDeclaration _ name kind ->
    "foreign import data"
      <+> pretty (runProperName name)
      <+> "::"
      <> line
      <> indent 2 (align $ fromKind kind)
      <> line
      <> line
  ExternDeclaration _ ident type' ->
    "foreign import"
      <+> pretty (runIdent ident)
      <+> "::"
      <> line
      <> indent 2 (align $ fromType type')
      <> line
      <> line
  ExternKindDeclaration _ name ->
    "foreign import kind" <+> pretty (runProperName name) <> line <> line
  FixityDeclaration _ (Left (ValueFixity fixity name op)) ->
    fromFixity fixity
      <+> pretty (showQualified (either runIdent runProperName) name)
      <+> "as"
      <+> pretty (runOpName op)
      <> line
      <> line
  FixityDeclaration _ (Right (TypeFixity fixity name op)) ->
    fromFixity fixity
      <+> "type"
      <+> pretty (showQualified runProperName name)
      <+> "as"
      <+> pretty (runOpName op)
      <> line
      <> line
  ImportDeclaration _ name importType qualified ->
    "import"
      <+> pretty (runModuleName name)
      <+> fromImportType importType
      <+> foldMap fromImportQualified qualified
      <> line
  TypeDeclaration TypeDeclarationData { tydeclIdent, tydeclType } ->
    pretty (runIdent tydeclIdent)
      <+> "::"
      <> line
      <> indent 2 (align $ fromType tydeclType)
      <> line
  TypeClassDeclaration _ name parameters [] funDeps declarations ->
    "class"
      <+> fromTypeClassWithoutConstraints name parameters funDeps declarations
      <> line
  TypeClassDeclaration _ name parameters constraints funDeps declarations ->
    "class"
      <> fromTypeClassConstraints "<=" constraints
      <> line
      <> indent 2 (fromTypeClassWithoutConstraints name parameters funDeps declarations)
      <> line
  TypeInstanceDeclaration _ ident constraints name types DerivedInstance ->
    "derive instance"
      <+> pretty (runIdent ident)
      <+> "::"
      <> fromTypeClassConstraints "=>" constraints
      <> line
      <> indent 2 ( pretty (showQualified runProperName name)
                  <+> hsep (map fromType types)
                  )
      <> line
      <> line
  TypeInstanceDeclaration _ ident constraints name types (ExplicitInstance declarations) ->
    "instance"
      <+> pretty (runIdent ident)
      <+> "::"
      <> fromTypeClassConstraints "=>" constraints
      <> line
      <> indent 2 ( pretty (showQualified runProperName name)
                  <+> hsep (map fromType types)
                  <+> "where"
                  <> line
                  <> indent 2 (vsep $ map fromDeclaration declarations)
                  )
      <> line
  TypeInstanceDeclaration _ ident constraints name types NewtypeInstance ->
    "derive newtype instance"
      <+> pretty (runIdent ident)
      <+> "::"
      <> fromTypeClassConstraints "=>" constraints
      <> line
      <> indent 2 ( pretty (showQualified runProperName name)
                  <+> hsep (map fromType types)
                  )
      <> line
      <> line
  TypeInstanceDeclaration _ ident constraints name types (NewtypeInstanceWithDictionary _) ->
    "derive newtype instance"
      <+> pretty (runIdent ident)
      <+> "::"
      <> fromTypeClassConstraints "=>" constraints
      <> line
      <> indent 2 ( pretty (showQualified runProperName name)
                  <+> hsep (map fromType types)
                  )
      <> line
      <> line
  TypeSynonymDeclaration _ name parameters underlyingType ->
    "type"
      <+> pretty (runProperName name)
      <+> fromParameters parameters
      <> line
      <> indent 2 ("=" <+> pretty (prettyPrintType underlyingType))
      <> line
  ValueDeclaration ValueDeclarationData { valdeclBinders, valdeclExpression, valdeclIdent } ->
    pretty (runIdent valdeclIdent)
      <> fromBinders valdeclBinders
      <+> foldMap fromGuardedExpr valdeclExpression
      <> line
      <> line

fromDeclarations :: [Declaration] -> Doc a
fromDeclarations = foldMap fromDeclaration

fromDoElement :: DoNotationElement -> Doc a
fromDoElement = \case
  DoNotationBind binder expr ->
    fromBinder binder <+> "<-" <+> fromExpr expr
  DoNotationLet declarations ->
    "let" <> line <> indent 4 (fromDeclarations declarations)
  DoNotationValue expr -> fromExpr expr
  PositionedDoNotationElement _ comments element ->
    foldMap fromComment comments <> fromDoElement element

fromExport :: DeclarationRef -> Doc a
fromExport = \case
  KindRef _ name -> "kind" <+> pretty (runProperName name)
  ModuleRef _ name -> "module" <+> pretty (runModuleName name)
  ReExportRef _ _ _ -> mempty
  TypeRef _ name constructors ->
    -- N.B. `Nothing` means everything
    pretty (runProperName name) <> maybe "(..)" fromConstructors constructors
  TypeClassRef _ name -> "class" <+> pretty (runProperName name)
  TypeInstanceRef _ _ -> mempty
  TypeOpRef _ name -> "type" <+> pretty (showOp name)
  ValueRef _ ident -> pretty (runIdent ident)
  ValueOpRef _ name -> pretty (showOp name)

fromExports :: [DeclarationRef] -> Doc a
fromExports = parentheses . map fromExport

fromExpr :: Expr -> Doc a
fromExpr = \case
  Abs binder expr ->
    "\\" <> fromBinder binder <+> "->" <> line <> indent 2 (fromExpr expr)
  Accessor key expr -> fromExpr expr <> "." <> pretty (prettyPrintString key)
  AnonymousArgument -> "_"
  App expr1 expr2 -> fromExpr expr1 <+> fromExpr expr2
  BinaryNoParens op left right ->
    fromExpr left <+> fromExpr op <+> fromExpr right
  Case exprs alternatives ->
    "case"
      <+> hsep (punctuate comma $ map fromExpr exprs)
      <+> "of"
      <> line
      <> indent 2 (vsep $ map fromCaseAlternative alternatives)
  Constructor name -> pretty (showQualified runProperName name)
  DeferredDictionary _ _ -> mempty
  Do elements ->
    "do"
      <> line
      <> indent 2 (vsep $ map fromDoElement elements)
  Hole hole -> "?" <> pretty hole
  IfThenElse b t f ->
    "if"
      <+> fromExpr b
      <+> "then"
      <> line
      <> indent 2 (fromExpr t)
      <> line
      <> indent (-2) "else"
      <> line
      <> indent 2 (fromExpr f)
  Let declarations expr ->
    line
      <> indent 2
        ( "let"
        <> line
        <> indent 2 (foldMap fromDeclaration declarations)
        <> line
        <> "in"
        <> line
        <> indent 2 (fromExpr expr)
        )
  Literal literal -> fromLiteral (map fromExpr literal)
  ObjectUpdate expr obj ->
    fromExpr expr <+> braces (map (fromObjectUpdate . map fromExpr) obj)
  ObjectUpdateNested expr pathTree ->
    fromExpr expr <+> fromPathTree pathTree
  Op op -> pretty (showQualified runOpName op)
  Parens expr -> parens (fromExpr expr)
  PositionedValue _ comments expr ->
    foldMap fromComment comments <> fromExpr expr
  TypeClassDictionary _ _ _ -> mempty
  TypeClassDictionaryAccessor _ _ -> mempty
  TypeClassDictionaryConstructorApp _ _ -> mempty
  TypedValue _ expr exprType ->
    fromExpr expr <+> "::" <+> pretty (prettyPrintType exprType)
  UnaryMinus expr -> "-" <> fromExpr expr
  Var ident -> pretty (showQualified runIdent ident)

fromFixity :: Language.PureScript.Fixity -> Doc a
fromFixity (Fixity associativity precedence) =
  pretty (showAssoc associativity) <+> pretty precedence

fromFunctionalDependencies :: IntMap Text -> [FunctionalDependency] -> Doc a
fromFunctionalDependencies vars = \case
  [] -> mempty
  funDeps ->
    space
      <> "|"
      <+> hsep (punctuate comma $ map (fromFunctionalDependency vars) funDeps)

fromFunctionalDependency :: IntMap Text -> FunctionalDependency -> Doc a
fromFunctionalDependency vars FunctionalDependency { fdDetermined, fdDeterminers } =
  hsep (map (pretty . flip (findWithDefault mempty) vars) fdDeterminers)
    <+> "->"
    <+> hsep (map (pretty . flip (findWithDefault mempty) vars) fdDetermined)

fromGuard :: Guard -> Doc a
fromGuard = \case
  ConditionGuard expr -> fromExpr expr
  PatternGuard binder expr -> fromBinder binder <+> "<-" <+> fromExpr expr

fromGuardedExpr :: GuardedExpr -> Doc a
fromGuardedExpr = fromGuardedExpr' "="

fromGuardedExprCase :: GuardedExpr -> Doc a
fromGuardedExprCase = fromGuardedExpr' "->"

fromGuardedExpr' :: Doc a -> GuardedExpr -> Doc a
fromGuardedExpr' separator (GuardedExpr [] expr) =
  separator <+> fromExpr expr
fromGuardedExpr' separator (GuardedExpr guards expr) =
  line <> indent 2 guardedExpr
  where
  guardedExpr =
    "|"
      <+> hsep (punctuate comma (map fromGuard guards))
      <+> separator
      <> line
      <> indent 2 (fromExpr expr)

fromImportQualified :: ModuleName -> Doc a
fromImportQualified name = "as" <+> pretty (runModuleName name)

fromImportType :: ImportDeclarationType -> Doc a
fromImportType = \case
  Explicit declarationRefs -> line <> indent 2 (fromExports declarationRefs)
  Hiding declarationRefs ->
    line <> indent 2 ("hiding" <+> fromExports declarationRefs)
  Implicit -> mempty

fromKind :: Kind -> Doc a
fromKind = pretty . prettyPrintKind

fromLiteral :: Literal (Doc a) -> Doc a
fromLiteral = \case
  ArrayLiteral xs -> brackets xs
  BooleanLiteral b -> pretty b
  CharLiteral c -> pretty c
  NumericLiteral (Left x) -> pretty x
  NumericLiteral (Right x) -> pretty x
  ObjectLiteral obj -> braces (map fromObject obj)
  StringLiteral str -> pretty (prettyPrintString str)

fromModule :: Module -> Doc a
fromModule (Module _ comments name declarations' exports) =
  foldMap fromComment comments
    <> fromModuleExports name exports
    <> line
    <> line
    <> fromDeclarations imports
    <> line
    <> fromDeclarations declarations
  where
  (imports, declarations) = span isImportDecl declarations'

fromModuleExports :: ModuleName -> Maybe [DeclarationRef] -> Doc ann
fromModuleExports name Nothing =
  "module" <+> pretty (runModuleName name) <+> "where"
fromModuleExports name (Just exports) =
  "module"
    <+> pretty (runModuleName name)
    <> line
    <> indent 2 (fromExports exports <> line <> "where")

fromObject :: (PSString, Doc a) -> Doc a
fromObject (key, val) = pretty (prettyPrintString key) <> ":" <+> val

fromObjectUpdate :: (PSString, Doc a) -> Doc a
fromObjectUpdate (key, val) = pretty (prettyPrintString key) <+> "=" <+> val

fromParameter :: (Text, Maybe Kind) -> Doc a
fromParameter (parameter, Nothing) = pretty parameter
fromParameter (parameter, Just k) =
  parens (pretty parameter <+> "::" <+> fromKind k)

fromParameters :: [(Text, Maybe Kind)] -> Doc a
fromParameters = hsep . map fromParameter

fromPathNode :: (PSString, PathNode Expr) -> Doc a
fromPathNode (key, Leaf expr) =
  pretty (prettyPrintString key) <+> "=" <+> fromExpr expr
fromPathNode (key, Branch path) =
  pretty (prettyPrintString key) <+> fromPathTree path

fromPathTree :: PathTree Expr -> Doc a
fromPathTree (PathTree paths) =
  braces (map fromPathNode $ runAssocList paths)

fromType :: Language.PureScript.Type -> Doc a
fromType =
  go . everywhereOnTypesTopDown (convertForAlls []) . everywhereOnTypes convertTypeApps
  where
  go = \case
    BinaryNoParensType op left right ->
      fromType left <+> fromType op <+> fromType right
    ConstrainedType constraint type' ->
      fromConstraint constraint <+> "=>" <> line <> fromType type'
    ForAll var type' _ ->
      "forall" <+> pretty var <> "." <> line <> fromType type'
    KindedType type' kind ->
      parens (fromType type' <+> "::" <+> fromKind kind)
    ParensInType type' -> parens (fromTypeWithParens type')
    PrettyPrintForAll [] type' -> fromType type'
    PrettyPrintForAll vars type' ->
      "forall"
        <+> hcat (punctuate space $ map pretty vars)
        <> "."
        <> line
        <> fromType type'
    PrettyPrintFunction f x -> fromType f <+> "->" <> line <> fromType x
    type'@PrettyPrintObject {} -> "{" <> hsep (convertRow [] type') <> "}"
    type'@RCons {} -> "(" <> hsep (convertRow [] type') <> ")"
    REmpty -> "()"
    Skolem _ _ _ _ -> mempty
    TUnknown _ -> mempty
    TypeApp f x -> fromType f <+> fromType x
    TypeConstructor constructor ->
      pretty (showQualified runProperName constructor)
    TypeLevelString str -> pretty (prettyPrintString str)
    TypeOp op -> pretty (showQualified runOpName op)
    TypeVar var -> pretty var
    TypeWildcard _ -> "_"

fromTypeWithParens :: Language.PureScript.Type -> Doc a
fromTypeWithParens =
  go . everywhereOnTypesTopDown (convertForAlls []) . everywhereOnTypes convertTypeApps
  where
  go = \case
    BinaryNoParensType op left right ->
      fromTypeWithParens left
        <+> fromTypeWithParens op
        <+> fromTypeWithParens right
    ConstrainedType constraint type' ->
      fromConstraint constraint <+> "=>" <+> fromTypeWithParens type'
    ForAll var type' _ ->
      "forall" <+> pretty var <> "." <+> fromTypeWithParens type'
    KindedType type' kind ->
      parens (fromTypeWithParens type' <+> "::" <+> fromKind kind)
    ParensInType type' -> parens (fromTypeWithParens type')
    PrettyPrintForAll [] type' -> fromTypeWithParens type'
    PrettyPrintForAll vars type' ->
      "forall"
        <+> hcat (punctuate space $ map pretty vars)
        <> "."
        <+> fromTypeWithParens type'
    PrettyPrintFunction f x ->
      fromTypeWithParens f <+> "->" <+> fromTypeWithParens x
    type'@PrettyPrintObject {} -> "{" <> hsep (convertRow [] type') <> "}"
    type'@RCons {} -> "(" <> hsep (convertRow [] type') <> ")"
    REmpty -> "()"
    Skolem _ _ _ _ -> mempty
    TUnknown _ -> mempty
    TypeApp f x -> fromTypeWithParens f <+> fromTypeWithParens x
    TypeConstructor constructor ->
      pretty (showQualified runProperName constructor)
    TypeLevelString str -> pretty (prettyPrintString str)
    TypeOp op -> pretty (showQualified runOpName op)
    TypeVar var -> pretty var
    TypeWildcard _ -> "_"

convertRow :: [Doc a] -> Language.PureScript.Type -> [Doc a]
convertRow rest = \case
  RCons label type' tail@REmpty ->
    convertRow
      ( pretty (prettyPrintString $ runLabel label)
      <+> "::"
      <+> fromType type'
      : rest
      )
      tail
  RCons label type' tail@RCons{} ->
    convertRow
      ( pretty (prettyPrintString $ runLabel label)
      <+> "::"
      <+> fromType type'
      <> ","
      : rest
      )
      tail
  RCons label type' tail ->
    convertRow
      ( pretty (prettyPrintString $ runLabel label)
      <+> "::"
      <+> fromType type'
      <+> "|"
      : rest
      )
      tail
  REmpty -> reverse rest
  x -> reverse (fromType x : rest)

convertTypeApps :: Language.PureScript.Type -> Language.PureScript.Type
convertTypeApps = \case
  TypeApp (TypeApp f g) x | f == tyFunction -> PrettyPrintFunction g x
  TypeApp o r | o == tyRecord -> PrettyPrintObject r
  x -> x

convertForAlls :: [Text] -> Language.PureScript.Type -> Language.PureScript.Type
convertForAlls vars = \case
  ForAll var (quantifiedType@ForAll {}) _ ->
    convertForAlls (var : vars) quantifiedType
  ForAll var quantifiedType _ -> PrettyPrintForAll (var : vars) quantifiedType
  other -> other

fromTypeClassConstraints :: Doc a -> [Language.PureScript.Constraint] -> Doc a
fromTypeClassConstraints arrow = \case
  [] -> mempty
  constraints ->
    space
      <> line
      <> indent 2 (align (cat (zipWith (<>) ("(" <> space : repeat ", ") (map fromConstraint constraints)) <> line <> ")"))
      <+> arrow

fromTypeClassWithoutConstraints ::
  ProperName 'ClassName ->
  [(Text, Maybe Kind)] ->
  [FunctionalDependency] ->
  [Declaration] ->
  Doc a
fromTypeClassWithoutConstraints name parameters funDeps declarations =
  pretty (runProperName name)
    <+> fromParameters parameters
    <> fromFunctionalDependencies (fromList $ zip [0..] $ map fst parameters) funDeps
    <+> "where"
    <> line
    <> indent 2 (vsep $ map fromDeclaration declarations)

parentheses :: [Doc a] -> Doc a
parentheses = enclosedWith "(" ")"

valueDeclarationFromAnonymousDeclaration ::
  ((SourceAnn, Ident), NameKind, Expr) ->
  ValueDeclarationData [GuardedExpr]
valueDeclarationFromAnonymousDeclaration ((valdeclSourceAnn, valdeclIdent), valdeclName, expr) =
  ValueDeclarationData
    { valdeclBinders
    , valdeclExpression
    , valdeclIdent
    , valdeclName
    , valdeclSourceAnn
    }
  where
  valdeclBinders = []
  valdeclExpression = [GuardedExpr [] expr]
