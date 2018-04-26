module Doc where

import "rio" RIO

import "containers" Data.IntMap.Strict           (findWithDefault, fromList)
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
    , nest
    , parens
    , pretty
    , punctuate
    , space
    , vsep
    , (<+>)
    )
import "purescript" Language.PureScript
    ( Binder
    , CaseAlternative(CaseAlternative, caseAlternativeBinders, caseAlternativeResult)
    , Comment(BlockComment, LineComment)
    , Constraint(Constraint, constraintArgs, constraintClass)
    , DataDeclType(Data, Newtype)
    , Declaration(BindingGroupDeclaration, BoundValueDeclaration, DataBindingGroupDeclaration, DataDeclaration, ExternDataDeclaration, ExternDeclaration, ExternKindDeclaration, FixityDeclaration, ImportDeclaration, TypeClassDeclaration, TypeDeclaration, TypeInstanceDeclaration, TypeSynonymDeclaration, ValueDeclaration)
    , DeclarationRef(KindRef, ModuleRef, ReExportRef, TypeClassRef, TypeInstanceRef, TypeOpRef, TypeRef, ValueOpRef, ValueRef)
    , DoNotationElement(DoNotationBind, DoNotationLet, DoNotationValue, PositionedDoNotationElement)
    , Expr(Abs, Accessor, AnonymousArgument, App, BinaryNoParens, Case, Constructor, DeferredDictionary, Do, Hole, IfThenElse, Let, Literal, ObjectUpdate, ObjectUpdateNested, Op, Parens, PositionedValue, TypeClassDictionary, TypeClassDictionaryAccessor, TypeClassDictionaryConstructorApp, TypedValue, UnaryMinus, Var)
    , Fixity(Fixity)
    , FunctionalDependency(FunctionalDependency, fdDetermined, fdDeterminers)
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
    , TypeDeclarationData(TypeDeclarationData, tydeclIdent, tydeclSourceAnn, tydeclType)
    , TypeFixity(TypeFixity)
    , TypeInstanceBody(DerivedInstance, ExplicitInstance, NewtypeInstance, NewtypeInstanceWithDictionary)
    , ValueDeclarationData(ValueDeclarationData, valdeclBinders, valdeclExpression, valdeclIdent, valdeclName, valdeclSourceAnn)
    , ValueFixity(ValueFixity)
    , everywhereOnTypes
    , everywhereOnTypesTopDown
    , isImportDecl
    , prettyPrintBinder
    , prettyPrintKind
    , prettyPrintString
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
    )
import "purescript" Language.PureScript.Label    (runLabel)
import "purescript" Language.PureScript.Names    (Qualified)
import "purescript" Language.PureScript.PSString (PSString)
import "rio" RIO.List                            (repeat, zipWith)
import "rio" RIO.Text                            (dropAround)

ppStringWithoutQuotes :: PSString -> Text
ppStringWithoutQuotes = dropAround (== '"') . prettyPrintString

braces :: [Doc a] -> Doc a
braces = enclosedWith "{" "}"

brackets :: [Doc a] -> Doc a
brackets = enclosedWith "[" "]"

convertForAlls :: [Text] -> Language.PureScript.Type -> Language.PureScript.Type
convertForAlls vars = \case
  ForAll var (quantifiedType@ForAll {}) _ ->
    convertForAlls (var : vars) quantifiedType
  ForAll var quantifiedType _ -> PrettyPrintForAll (var : vars) quantifiedType
  other -> other

convertRow :: [Doc a] -> Language.PureScript.Type -> [Doc a]
convertRow rest = \case
  RCons label type' tail@RCons{} ->
    convertRow
      ( printRowPair label type'
      <> ","
      : rest
      )
      tail
  RCons label type' tail ->
    convertRow
      ( printRowPair label type'
      : rest
      )
      tail
  REmpty -> reverse rest
  x -> reverse ("|" <+> fromType x : rest)
  where
    printRowPair l t = pretty (ppStringWithoutQuotes $ runLabel l) <+> "::" <+> fromType t

convertTypeApps :: Language.PureScript.Type -> Language.PureScript.Type
convertTypeApps = \case
  TypeApp (TypeApp f g) x | f == tyFunction -> PrettyPrintFunction g x
  TypeApp o r | o == tyRecord -> PrettyPrintObject r
  x -> x

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
  binders -> space <> hsep (fmap fromBinder binders)

fromCaseAlternative :: CaseAlternative -> Doc a
fromCaseAlternative CaseAlternative {caseAlternativeBinders, caseAlternativeResult} =
  hsep (punctuate comma $ fmap fromBinder caseAlternativeBinders)
    <+> foldMap fromGuardedExprCase caseAlternativeResult

fromComment :: Comment -> Doc a
fromComment = \case
  BlockComment comment -> enclose "{-" "-}" (pretty comment) <> line
  LineComment comment -> "--" <> pretty comment <> line

fromComments :: (Foldable f) => f Comment -> Doc a
fromComments = foldMap fromComment

fromConstraint :: Language.PureScript.Constraint -> Doc a
fromConstraint Constraint { constraintArgs, constraintClass } =
  pretty (showQualified runProperName constraintClass)
    <+> hsep (fmap fromType constraintArgs)

fromConstructor :: ProperName 'ConstructorName -> Doc a
fromConstructor = pretty . runProperName

fromConstructors :: [ProperName 'ConstructorName] -> Doc a
fromConstructors [] = mempty
fromConstructors constructors =
  parentheses $ fmap fromConstructor constructors

fromDataConstructor :: (ProperName 'ConstructorName, [Language.PureScript.Type]) -> Doc a
fromDataConstructor = \case
  (name, []) -> pretty (runProperName name)
  (name, types) -> pretty (runProperName name) <+> hsep (fmap fromType types)

fromDataConstructors :: [(ProperName 'ConstructorName, [Language.PureScript.Type])] -> Doc a
fromDataConstructors =
  vsep . zipWith (<+>) ("=" : repeat "|") . fmap fromDataConstructor

fromDataType :: DataDeclType -> Doc a
fromDataType = \case
  Data -> "data"
  Newtype -> "newtype"

fromDeclaration :: Declaration -> Doc a
fromDeclaration = \case
  BindingGroupDeclaration declarations ->
    vsep (toList $ fmap (fromDeclaration . ValueDeclaration . valueDeclarationFromAnonymousDeclaration) declarations)
  BoundValueDeclaration (_, comments) binder expr ->
    fromComments comments <> fromBinder binder <+> "=" <+> fromExpr expr
  DataBindingGroupDeclaration declarations ->
    vsep (toList $ fmap fromDeclaration declarations)
  DataDeclaration (_, comments) dataType name parameters constructors ->
    fromComments comments
      <> fromDataType dataType
      <+> pretty (runProperName name)
      <> fromParameters parameters
      <> line
      <> indent 2 (fromDataConstructors constructors)
      <> line
  ExternDataDeclaration (_, comments) name kind ->
    fromComments comments
      <> "foreign import data"
      <+> pretty (runProperName name)
      <+> "::"
      <> line
      <> indent 2 (align $ fromKind kind)
      <> line
  ExternDeclaration (_, comments) ident type' ->
    fromComments comments
      <> "foreign import"
      <+> pretty (runIdent ident)
      <+> "::"
      <> line
      <> indent 2 (align $ fromType type')
      <> line
  ExternKindDeclaration (_, comments) name ->
    fromComments comments
      <> "foreign import kind"
      <+> pretty (runProperName name)
      <> line
  FixityDeclaration (_, comments) (Left (ValueFixity fixity name op)) ->
    fromComments comments
      <> fromFixity fixity
      <+> pretty (showQualified (either runIdent runProperName) name)
      <+> "as"
      <+> pretty (runOpName op)
      <> line
  FixityDeclaration (_, comments) (Right (TypeFixity fixity name op)) ->
    fromComments comments
      <> fromFixity fixity
      <+> "type"
      <+> pretty (showQualified runProperName name)
      <+> "as"
      <+> pretty (runOpName op)
      <> line
  ImportDeclaration (_, comments) name importType qualified ->
    fromComments comments
      <> "import"
      <+> pretty (runModuleName name)
      <> fromImportType importType
      <+> foldMap fromImportQualified qualified
  TypeDeclaration TypeDeclarationData { tydeclIdent, tydeclSourceAnn = (_, comments), tydeclType } ->
    fromComments comments
      <> pretty (runIdent tydeclIdent)
      <+> "::"
      <> line
      <> indent 2 (align $ fromType tydeclType)
  TypeClassDeclaration (_, comments) name parameters [] funDeps declarations ->
    fromComments comments
      <> "class"
      <+> fromTypeClassWithoutConstraints name parameters funDeps declarations
  TypeClassDeclaration (_, comments) name parameters constraints funDeps declarations ->
    fromComments comments
      <> "class"
      <> fromTypeClassConstraints "<=" constraints
      <> line
      <> indent 2 (fromTypeClassWithoutConstraints name parameters funDeps declarations)
  TypeInstanceDeclaration (_, comments) ident constraints name types DerivedInstance ->
    fromComments comments
      <> "derive instance"
      <+> pretty (runIdent ident)
      <+> "::"
      <> fromTypeClassConstraints "=>" constraints
      <> line
      <> indent 2 ( pretty (showQualified runProperName name)
                  <+> hsep (fmap fromType types)
                  )
      <> line
  TypeInstanceDeclaration (_, comments) ident constraints name types (ExplicitInstance declarations) ->
    fromComments comments
      <> "instance"
      <+> pretty (runIdent ident)
      <+> "::"
      <> fromTypeClassConstraints "=>" constraints
      <> line
      <> indent 2 (fromTypeInstanceWithoutConstraints name types declarations)
  TypeInstanceDeclaration (_, comments) ident constraints name types NewtypeInstance ->
    fromComments comments
      <> "derive newtype instance"
      <+> pretty (runIdent ident)
      <+> "::"
      <> fromTypeClassConstraints "=>" constraints
      <> line
      <> indent 2 ( pretty (showQualified runProperName name)
                  <+> hsep (fmap fromType types)
                  )
      <> line
  TypeInstanceDeclaration (_, comments) ident constraints name types (NewtypeInstanceWithDictionary _) ->
    fromComments comments
      <> "derive newtype instance"
      <+> pretty (runIdent ident)
      <+> "::"
      <> fromTypeClassConstraints "=>" constraints
      <> line
      <> indent 2 ( pretty (showQualified runProperName name)
                  <+> hsep (fmap fromType types)
                  )
      <> line
  TypeSynonymDeclaration (_, comments) name parameters underlyingType ->
    fromComments comments
      <> "type"
      <+> pretty (runProperName name)
      <> fromParameters parameters
      <> line
      <> indent 2 ("=" <+> fromType underlyingType)
      <> line
  ValueDeclaration ValueDeclarationData { valdeclBinders, valdeclExpression, valdeclIdent, valdeclSourceAnn = (_, comments) } ->
    fromComments comments
      <> pretty (runIdent valdeclIdent)
      <> fromBinders valdeclBinders
      <+> nest 2 (foldMap fromGuardedExpr valdeclExpression)
      <> line

fromDeclarations :: [Declaration] -> Doc a
fromDeclarations = foldMap fromDeclaration

fromDoElement :: DoNotationElement -> Doc a
fromDoElement = \case
  DoNotationBind binder expr ->
    fromBinder binder <+> "<-" <+> fromExpr expr
  DoNotationLet declarations ->
    "let" <+> align (fromDeclarations declarations)
  DoNotationValue expr -> fromExpr expr
  PositionedDoNotationElement _ comments element ->
    fromComments comments <> fromDoElement element

fromExport :: DeclarationRef -> Doc a
fromExport = \case
  KindRef _ name -> "kind" <+> pretty (runProperName name)
  ModuleRef _ name -> "module" <+> pretty (runModuleName name)
  ReExportRef {} -> mempty
  TypeRef _ name constructors ->
    -- N.B. `Nothing` means everything
    pretty (runProperName name) <> maybe "(..)" fromConstructors constructors
  TypeClassRef _ name -> "class" <+> pretty (runProperName name)
  TypeInstanceRef _ _ -> mempty
  TypeOpRef _ name -> "type" <+> pretty (showOp name)
  ValueRef _ ident -> pretty (runIdent ident)
  ValueOpRef _ name -> pretty (showOp name)

fromExports :: [DeclarationRef] -> Doc a
fromExports = parentheses . fmap fromExport

fromExpr :: Expr -> Doc a
fromExpr = \case
  Abs binder expr ->
    "\\" <> fromBinder binder <+> "->" <> line <> indent 2 (fromExpr expr)
  Accessor key expr -> fromExpr expr <> "." <> pretty (ppStringWithoutQuotes key)
  AnonymousArgument -> "_"
  App expr1 expr2 -> fromExpr expr1 <+> fromExpr expr2
  BinaryNoParens op left right ->
    fromExpr left <+> fromExpr op <+> fromExpr right
  Case exprs alternatives ->
    "case"
      <+> hsep (punctuate comma $ fmap fromExpr exprs)
      <+> "of"
      <> line
      <> indent 2 (vsep $ fmap fromCaseAlternative alternatives)
  Constructor name -> pretty (showQualified runProperName name)
  DeferredDictionary _ _ -> mempty
  Do elements ->
    "do"
      <> line
      <> indent 2 (vsep $ fmap fromDoElement elements)
  Hole hole -> "?" <> pretty hole
  IfThenElse b t f ->
    align $ vsep
      [ "if" <+> fromExpr b <+> "then" <> line <> indent 2 (fromExpr t)
      , "else" <> line <> indent 2 (fromExpr f)
      ]
  Let declarations expr ->
    align $ vsep
      [ "let" <+> align (fromDeclarations declarations)
      , "in" <+> fromExpr expr
      ]
  Literal literal -> fromLiteral (fmap fromExpr literal)
  ObjectUpdate expr obj ->
    fromExpr expr <+> braces (fmap (fromObjectUpdate . fmap fromExpr) obj)
  ObjectUpdateNested expr pathTree ->
    fromExpr expr <+> fromPathTree pathTree
  Op op -> pretty (showQualified runOpName op)
  Parens expr -> parens (fromExpr expr)
  PositionedValue _ comments expr -> fromComments comments <> fromExpr expr
  TypeClassDictionary {} -> mempty
  TypeClassDictionaryAccessor _ _ -> mempty
  TypeClassDictionaryConstructorApp _ _ -> mempty
  TypedValue _ expr exprType -> fromExpr expr <+> "::" <+> fromType exprType
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
      <+> hsep (punctuate comma $ fmap (fromFunctionalDependency vars) funDeps)

fromFunctionalDependency :: IntMap Text -> FunctionalDependency -> Doc a
fromFunctionalDependency vars FunctionalDependency { fdDetermined, fdDeterminers } =
  hsep (fmap (pretty . flip (findWithDefault mempty) vars) fdDeterminers)
    <+> "->"
    <+> hsep (fmap (pretty . flip (findWithDefault mempty) vars) fdDetermined)

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
      <+> hsep (punctuate comma (fmap fromGuard guards))
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
  ObjectLiteral obj -> braces (fmap fromObject obj)
  StringLiteral str -> pretty (prettyPrintString str)

fromModule :: Module -> Doc a
fromModule (Module _ comments name declarations' exports) =
  fromComments comments
    <> fromModuleExports name exports
    <> fromModuleImports imports
    <> fromModuleDeclarations declarations
  where
  (imports, declarations) = span isImportDecl declarations'

fromModuleDeclarations :: [Declaration] -> Doc a
fromModuleDeclarations = \case
  [] -> mempty
  declarations -> line <> line <> vsep (fmap fromDeclaration declarations)

fromModuleExports :: ModuleName -> Maybe [DeclarationRef] -> Doc ann
fromModuleExports name Nothing =
  "module" <+> pretty (runModuleName name) <+> "where"
fromModuleExports name (Just exports) =
  "module"
    <+> pretty (runModuleName name)
    <> line
    <> indent 2 (fromExports exports <> line <> "where")

fromModuleImports :: [Declaration] -> Doc a
fromModuleImports = \case
  [] -> mempty
  imports -> line <> line <> vsep (fmap fromDeclaration imports)

fromObject :: (PSString, Doc a) -> Doc a
fromObject (key, val) = pretty (ppStringWithoutQuotes key) <> ":" <+> val

fromObjectUpdate :: (PSString, Doc a) -> Doc a
fromObjectUpdate (key, val) = pretty (ppStringWithoutQuotes key) <+> "=" <+> val

fromParameter :: (Text, Maybe Kind) -> Doc a
fromParameter (parameter, Nothing) = pretty parameter
fromParameter (parameter, Just k) =
  parens (pretty parameter <+> "::" <+> fromKind k)

fromParameters :: [(Text, Maybe Kind)] -> Doc a
fromParameters = \case
  [] -> mempty
  parameters -> space <> hsep (fmap fromParameter parameters)

fromPathNode :: (PSString, PathNode Expr) -> Doc a
fromPathNode (key, Leaf expr) =
  pretty (ppStringWithoutQuotes key) <+> "=" <+> fromExpr expr
fromPathNode (key, Branch path) =
  pretty (ppStringWithoutQuotes key) <+> fromPathTree path

fromPathTree :: PathTree Expr -> Doc a
fromPathTree (PathTree paths) =
  braces (fromPathNode <$> runAssocList paths)

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
        <+> hcat (punctuate space $ fmap pretty vars)
        <> "."
        <> line
        <> fromType type'
    PrettyPrintFunction f x -> fromType f <+> "->" <> line <> fromType x
    PrettyPrintObject type' -> hsep $ ["{"] <> convertRow [] type' <> ["}"]
    type'@RCons {} -> hsep $ ["("] <> convertRow [] type' <> [")"]
    REmpty -> "()"
    Skolem {} -> mempty
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
        <+> hcat (punctuate space $ fmap pretty vars)
        <> "."
        <+> fromTypeWithParens type'
    PrettyPrintFunction f x ->
      fromTypeWithParens f <+> "->" <+> fromTypeWithParens x
    PrettyPrintObject type' -> "{" <> hsep (convertRow [] type') <> "}"
    type'@RCons {} -> "(" <> hsep (convertRow [] type') <> ")"
    REmpty -> "()"
    Skolem {} -> mempty
    TUnknown _ -> mempty
    TypeApp f x -> fromTypeWithParens f <+> fromTypeWithParens x
    TypeConstructor constructor ->
      pretty (showQualified runProperName constructor)
    TypeLevelString str -> pretty (prettyPrintString str)
    TypeOp op -> pretty (showQualified runOpName op)
    TypeVar var -> pretty var
    TypeWildcard _ -> "_"

fromTypeClassConstraints :: Doc a -> [Language.PureScript.Constraint] -> Doc a
fromTypeClassConstraints arrow = \case
  [] -> mempty
  constraints ->
    space
      <> line
      <> indent 2 (align (cat (zipWith (<>) ("(" <> space : repeat ", ") (fmap fromConstraint constraints)) <> line <> ")"))
      <+> arrow

fromTypeClassWithoutConstraints ::
  ProperName 'ClassName ->
  [(Text, Maybe Kind)] ->
  [FunctionalDependency] ->
  [Declaration] ->
  Doc a
fromTypeClassWithoutConstraints name parameters funDeps declarations =
  case declarations of
    [] -> classHead
    _ -> classHead
      <+> "where"
      <> line
      <> indent 2 (vsep $ fmap fromDeclaration declarations)
  where classHead = pretty (runProperName name)
                      <> fromParameters parameters
                      <> fromFunctionalDependencies (fromList $ zip [0..] $ fmap fst parameters) funDeps

fromTypeInstanceWithoutConstraints ::
  Qualified (ProperName a) ->
  [Type] ->
  [Declaration] ->
  Doc ann
fromTypeInstanceWithoutConstraints name types declarations =
  case declarations of
    [] -> instanceHead
    _ -> instanceHead
      <+> "where"
      <> line
      <> indent 2 (vsep $ fmap fromDeclaration declarations)
  where instanceHead = pretty (showQualified runProperName name)
                        <+> hsep (fmap fromType types)

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
