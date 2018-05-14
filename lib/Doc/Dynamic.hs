-- N.B. The only reason we have an explicit export list is to get additional
-- analysis on the module, like unused binds.
-- Don't mis-interpret the export list as saying what the private/public API is.
-- If you need something from this module, export it.
module Doc.Dynamic
  ( fromLiteralBinder
  , fromLiteralExpr
  , fromModule
  , fromType
  ) where

import "rio" RIO

import "containers" Data.IntMap.Strict           (fromList)
import "prettyprinter" Data.Text.Prettyprint.Doc
    ( Doc
    , align
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
    ( Binder(BinaryNoParensBinder, ConstructorBinder, LiteralBinder, NamedBinder, NullBinder, OpBinder, ParensInBinder, PositionedBinder, TypedBinder, VarBinder)
    , CaseAlternative(CaseAlternative, caseAlternativeBinders, caseAlternativeResult)
    , Constraint(Constraint, constraintArgs, constraintClass)
    , Declaration(BindingGroupDeclaration, BoundValueDeclaration, DataBindingGroupDeclaration, DataDeclaration, ExternDataDeclaration, ExternDeclaration, ExternKindDeclaration, FixityDeclaration, ImportDeclaration, TypeClassDeclaration, TypeDeclaration, TypeInstanceDeclaration, TypeSynonymDeclaration, ValueDeclaration)
    , DeclarationRef(KindRef, ModuleRef, ReExportRef, TypeClassRef, TypeInstanceRef, TypeOpRef, TypeRef, ValueOpRef, ValueRef)
    , DoNotationElement(DoNotationBind, DoNotationLet, DoNotationValue, PositionedDoNotationElement)
    , Expr(Abs, Accessor, AnonymousArgument, App, BinaryNoParens, Case, Constructor, DeferredDictionary, Do, Hole, IfThenElse, Let, Literal, ObjectUpdate, ObjectUpdateNested, Op, Parens, PositionedValue, TypeClassDictionary, TypeClassDictionaryAccessor, TypeClassDictionaryConstructorApp, TypedValue, UnaryMinus, Var)
    , FunctionalDependency
    , Guard(ConditionGuard, PatternGuard)
    , GuardedExpr(GuardedExpr)
    , Ident(Ident)
    , ImportDeclarationType(Explicit, Hiding, Implicit)
    , Kind
    , Literal(ArrayLiteral, BooleanLiteral, CharLiteral, NumericLiteral, ObjectLiteral, StringLiteral)
    , Module(Module)
    , ModuleName
    , PathNode(Branch, Leaf)
    , PathTree(PathTree)
    , ProperName
    , ProperNameType(ClassName, ConstructorName)
    , Type(BinaryNoParensType, ConstrainedType, ForAll, KindedType, ParensInType, PrettyPrintForAll, PrettyPrintFunction, PrettyPrintObject, RCons, REmpty, Skolem, TUnknown, TypeApp, TypeConstructor, TypeLevelString, TypeOp, TypeVar, TypeWildcard)
    , TypeDeclarationData(TypeDeclarationData, tydeclIdent, tydeclSourceAnn, tydeclType)
    , TypeFixity(TypeFixity)
    , TypeInstanceBody(DerivedInstance, ExplicitInstance, NewtypeInstance, NewtypeInstanceWithDictionary)
    , ValueDeclarationData(ValueDeclarationData, valdeclBinders, valdeclExpression, valdeclIdent, valdeclSourceAnn)
    , ValueFixity(ValueFixity)
    , everywhereOnTypes
    , everywhereOnTypesTopDown
    , isImportDecl
    , prettyPrintString
    , runAssocList
    , runIdent
    , runModuleName
    , runOpName
    , runProperName
    , showOp
    , showQualified
    )
import "purescript" Language.PureScript.Label    (runLabel)
import "purescript" Language.PureScript.Names    (Qualified(Qualified))
import "purescript" Language.PureScript.PSString (PSString, mkString)
import "rio" RIO.List                            (repeat, zipWith)

import "this" Doc
    ( convertForAlls
    , convertTypeApps
    , fromComments
    , fromConstructors
    , fromDataType
    , fromFixity
    , fromFunctionalDependencies
    , fromImportQualified
    , fromKind
    , fromObjectUpdate
    , fromPSString
    , fromPSString
    , fromParameters
    , partitionImports
    , valueDeclarationFromAnonymousDeclaration
    )

braces :: [Doc a] -> Doc a
braces = enclosedWith "{" "}"

brackets :: [Doc a] -> Doc a
brackets = enclosedWith "[" "]"

convertRow :: [Doc a] -> Language.PureScript.Type -> [Doc a]
convertRow rest = \case
  RCons label type' tail@RCons{} ->
    convertRow
      ( printRowPair label type'
      <> ","
      <> space
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
  x -> reverse (space <> "|" <+> fromType x : rest)
  where
    printRowPair l t = fromPSString (runLabel l) <+> "::" <+> fromType t

enclosedWith :: Doc a -> Doc a -> [Doc a] -> Doc a
enclosedWith open close =
  align
    . group
    . encloseSep
      (flatAlt (open <> space) open)
      (flatAlt (line <> close) close)
      ", "

fromBinaryOp :: Expr -> Doc a
fromBinaryOp = \case
  Op op -> pretty (showQualified runOpName op)
  PositionedValue _ comments expr -> fromComments comments <> fromBinaryOp expr
  expr -> fromExpr expr

fromBinder :: Binder -> Doc a
fromBinder = \case
  BinaryNoParensBinder left op right ->
    fromBinder left <+> fromBinder op <+> fromBinder right
  ConstructorBinder name [] -> pretty (showQualified runProperName name)
  ConstructorBinder name binders ->
    pretty (showQualified runProperName name) <+> hsep (fmap fromBinder binders)
  LiteralBinder literal -> fromLiteralBinder literal
  NamedBinder name binder -> pretty (runIdent name) <> "@" <> fromBinder binder
  NullBinder -> "_"
  OpBinder name -> pretty (showQualified runOpName name)
  ParensInBinder binder -> enclose "(" ")" (fromBinder binder)
  PositionedBinder _ comments binder ->
    fromComments comments <> fromBinder binder
  TypedBinder type' binder -> fromBinder binder <+> "::" <+> fromType type'
  VarBinder ident -> pretty (runIdent ident)

fromBinders :: [Binder] -> Doc a
fromBinders = \case
  [] -> mempty
  binders -> space <> hsep (fmap fromBinder binders)

fromCaseAlternative :: CaseAlternative -> Doc a
fromCaseAlternative CaseAlternative {caseAlternativeBinders, caseAlternativeResult} =
  hsep (punctuate comma $ fmap fromBinder caseAlternativeBinders)
    <+> foldMap fromGuardedExprCase caseAlternativeResult

fromConstraint :: Language.PureScript.Constraint -> Doc a
fromConstraint Constraint { constraintArgs, constraintClass } =
  pretty (showQualified runProperName constraintClass)
    <+> hsep (fmap fromType constraintArgs)

fromDataConstructor :: (ProperName 'ConstructorName, [Language.PureScript.Type]) -> Doc a
fromDataConstructor = \case
  (name, []) -> pretty (runProperName name)
  (name, types) -> pretty (runProperName name) <+> hsep (fmap fromType types)

fromDataConstructors :: [(ProperName 'ConstructorName, [Language.PureScript.Type])] -> Doc a
fromDataConstructors =
  vsep . zipWith (<+>) ("=" : repeat "|") . fmap fromDataConstructor

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
      <> group
        (flatAlt
          (line <> indent 2 (align $ fromKind kind))
          (space <> fromKind kind)
        )
      <> line
  ExternDeclaration (_, comments) ident type' ->
    fromComments comments
      <> "foreign import"
      <+> pretty (runIdent ident)
      <+> "::"
      <> group
        (flatAlt
          (line <> indent 2 (align $ fromType type'))
          (space <> fromType type')
        )
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
      <> foldMap fromImportQualified qualified
  TypeDeclaration TypeDeclarationData { tydeclIdent, tydeclSourceAnn = (_, comments), tydeclType } ->
    fromComments comments
      <> pretty (runIdent tydeclIdent)
      <+> "::"
      <> group
        (flatAlt
          (line <> indent 2 (align $ fromType tydeclType))
          (space <> fromType tydeclType)
        )
  TypeClassDeclaration (_, comments) name parameters constraints funDeps declarations ->
    fromComments comments
      <> "class"
      <> fromTypeClassConstraints
        "<="
        (fromTypeClassWithoutConstraints name parameters funDeps declarations)
        constraints
  TypeInstanceDeclaration (_, comments) ident constraints name types DerivedInstance ->
    fromComments comments
      <> "derive instance"
      <+> pretty (runIdent ident)
      <+> "::"
      <> fromTypeClassConstraints
        "=>"
        ( pretty (showQualified runProperName name)
        <+> hsep (fmap fromType types)
        )
        constraints
      <> line
  TypeInstanceDeclaration (_, comments) ident constraints name types (ExplicitInstance declarations) ->
    fromComments comments
      <> "instance"
      <+> pretty (runIdent ident)
      <+> "::"
      <> fromTypeClassConstraints
        "=>"
        (fromTypeInstanceWithoutConstraints name types declarations)
        constraints
  TypeInstanceDeclaration (_, comments) ident constraints name types NewtypeInstance ->
    fromComments comments
      <> "derive newtype instance"
      <+> pretty (runIdent ident)
      <+> "::"
      <> fromTypeClassConstraints
        "=>"
        ( pretty (showQualified runProperName name)
        <+> hsep (fmap fromType types)
        )
        constraints
      <> line
  TypeInstanceDeclaration (_, comments) ident constraints name types (NewtypeInstanceWithDictionary _) ->
    fromComments comments
      <> "derive newtype instance"
      <+> pretty (runIdent ident)
      <+> "::"
      <> fromTypeClassConstraints
        "=>"
        ( pretty (showQualified runProperName name)
        <+> hsep (fmap fromType types)
        )
        constraints
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
  Accessor key expr -> fromExpr expr <> "." <> fromPSString key
  AnonymousArgument -> "_"
  App expr1 expr2 -> fromExpr expr1 <+> fromExpr expr2
  BinaryNoParens op left right ->
    fromExpr left <+> fromBinaryOp op <+> fromExpr right
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
  Literal literal -> fromLiteralExpr literal
  ObjectUpdate expr obj ->
    fromExpr expr <+> braces (fmap (fromObjectUpdate . fmap fromExpr) obj)
  ObjectUpdateNested expr pathTree ->
    fromExpr expr <+> fromPathTree pathTree
  Op op -> enclose "(" ")" (pretty $ showQualified runOpName op)
  Parens expr -> parens (fromExpr expr)
  PositionedValue _ comments expr -> fromComments comments <> fromExpr expr
  TypeClassDictionary {} -> mempty
  TypeClassDictionaryAccessor _ _ -> mempty
  TypeClassDictionaryConstructorApp _ _ -> mempty
  TypedValue _ expr exprType -> fromExpr expr <+> "::" <+> fromType exprType
  UnaryMinus expr -> "-" <> fromExpr expr
  Var ident -> pretty (showQualified runIdent ident)

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

fromImportType :: ImportDeclarationType -> Doc a
fromImportType = \case
  Explicit declarationRefs ->
    flatAlt
      (space <> fromExports declarationRefs)
      (line <> indent 2 (fromExports declarationRefs))
  Hiding declarationRefs ->
    flatAlt
      (space <> "hiding" <+> fromExports declarationRefs)
      (line <> indent 2 ("hiding" <+> fromExports declarationRefs))
  Implicit -> mempty

fromLiteralBinder :: Literal Binder -> Doc a
fromLiteralBinder = \case
  ArrayLiteral xs ->
    enclose "[" "]" (hsep $ punctuate comma $ fmap fromBinder xs)
  BooleanLiteral b -> pretty b
  CharLiteral c -> pretty c
  NumericLiteral (Left x) -> pretty x
  NumericLiteral (Right x) -> pretty x
  ObjectLiteral obj ->
    enclose "{" "}" (hsep $ punctuate comma $ fmap fromObjectBinder obj)
  StringLiteral str -> pretty (prettyPrintString str)

fromLiteralExpr :: Literal Expr -> Doc a
fromLiteralExpr = \case
  ArrayLiteral xs -> brackets (fmap fromExpr xs)
  BooleanLiteral b -> pretty b
  CharLiteral c -> pretty c
  NumericLiteral (Left x) -> pretty x
  NumericLiteral (Right x) -> pretty x
  ObjectLiteral obj -> braces (fmap fromObjectExpr obj)
  StringLiteral str -> pretty (prettyPrintString str)

fromModule :: Module -> Doc a
fromModule (Module _ comments name declarations' exports) =
  fromComments comments
    <> fromModuleExports name exports
    <> fromModuleImports openImports
    <> fromModuleImports explicitImports
    <> fromModuleImports qualifiedImports
    <> fromModuleDeclarations declarations
  where
  (imports, declarations) = span isImportDecl declarations'
  (openImports, explicitImports, qualifiedImports) = partitionImports imports

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

fromObjectBinder :: (PSString, Binder) -> Doc a
fromObjectBinder = \case
  (key, PositionedBinder _ comments binder) ->
    fromComments comments <> fromObjectBinder (key, binder)
  (key, VarBinder (Ident ident))
    | key == mkString ident -> pretty ident
  (key, val) -> fromPSString key <> ":" <+> fromBinder val

fromObjectExpr :: (PSString, Expr) -> Doc a
fromObjectExpr = \case
  (key, PositionedValue _ comments expr) ->
    fromComments comments <> fromObjectExpr (key, expr)
  (key, Var (Qualified Nothing (Ident ident)))
    | key == mkString ident -> pretty ident
  (key, val) -> fromPSString key <> ":" <+> fromExpr val

fromPathNode :: (PSString, PathNode Expr) -> Doc a
fromPathNode (key, Leaf expr) =
  fromPSString key <+> "=" <+> fromExpr expr
fromPathNode (key, Branch path) =
  fromPSString key <+> fromPathTree path

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
    PrettyPrintFunction f x ->
      flatAlt
        (fromType f <+> "->" <> line <> fromType x)
        (fromType f <+> "->" <+> fromType x)
    PrettyPrintObject type' -> "{" <> hcat (convertRow [] type') <> "}"
    type'@RCons {} -> "(" <> hcat (convertRow [] type') <> ")"
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
    PrettyPrintObject type' -> "{" <> hcat (convertRow [] type') <> "}"
    type'@RCons {} -> "(" <> hcat (convertRow [] type') <> ")"
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

fromTypeClassConstraints :: Doc a -> Doc a -> [Language.PureScript.Constraint] -> Doc a
fromTypeClassConstraints arrow rest = \case
  [] -> space <> rest
  constraints ->
    flatAlt
      ( line
      <> indent 2 ( align (vsep (zipWith (<+>) ("(" : repeat comma) $ fmap fromConstraint constraints) <> line <> ")" <+> arrow)
                  <> line
                  <> rest
                  )
      )
      ( space
      <> "("
      <> hsep (punctuate comma $ fmap fromConstraint constraints)
      <> ")"
      <+> arrow
      <+> rest
      )

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
