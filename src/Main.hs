module Main where

import "protolude" Protolude hiding (group)

import "base" Data.List                                      (span)
import "prettyprinter" Data.Text.Prettyprint.Doc
    ( Doc
    , LayoutOptions
    , align
    , comma
    , defaultLayoutOptions
    , enclose
    , encloseSep
    , flatAlt
    , group
    , hcat
    , hsep
    , indent
    , layoutSmart
    , line
    , parens
    , pretty
    , punctuate
    , sep
    , space
    , vsep
    , (<+>)
    )
import "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text (renderIO)
import "purescript" Language.PureScript
    ( Binder
    , CaseAlternative(CaseAlternative)
    , Comment(BlockComment, LineComment)
    , Constraint(Constraint)
    , DataDeclType(Data, Newtype)
    , Declaration(DataBindingGroupDeclaration, DataDeclaration, ImportDeclaration, TypeDeclaration, TypeSynonymDeclaration, ValueDeclaration)
    , DeclarationRef(KindRef, ModuleRef, ReExportRef, TypeClassRef, TypeInstanceRef, TypeOpRef, TypeRef, ValueOpRef, ValueRef)
    , DoNotationElement(DoNotationBind, DoNotationLet, DoNotationValue, PositionedDoNotationElement)
    , Expr(Abs, Accessor, AnonymousArgument, App, BinaryNoParens, Case, Constructor, DeferredDictionary, Do, Hole, IfThenElse, Let, Literal, ObjectUpdate, ObjectUpdateNested, Op, Parens, PositionedValue, TypeClassDictionary, TypeClassDictionaryAccessor, TypeClassDictionaryConstructorApp, TypedValue, UnaryMinus, Var)
    , Guard(ConditionGuard, PatternGuard)
    , GuardedExpr(GuardedExpr)
    , ImportDeclarationType(Explicit, Hiding, Implicit)
    , Kind
    , Literal(ArrayLiteral, BooleanLiteral, CharLiteral, NumericLiteral, ObjectLiteral, StringLiteral)
    , Module(Module)
    , ModuleName
    , PathNode(Branch, Leaf)
    , PathTree(PathTree)
    , ProperName
    , ProperNameType(ConstructorName)
    , Type(BinaryNoParensType, ConstrainedType, ForAll, KindedType, ParensInType, PrettyPrintForAll, PrettyPrintFunction, PrettyPrintObject, RCons, REmpty, Skolem, TUnknown, TypeApp, TypeConstructor, TypeLevelString, TypeOp, TypeVar, TypeWildcard)
    , TypeDeclarationData(TypeDeclarationData)
    , ValueDeclarationData(ValueDeclarationData)
    , caseAlternativeBinders
    , caseAlternativeResult
    , constraintArgs
    , constraintClass
    , everywhereOnTypes
    , everywhereOnTypesTopDown
    , isImportDecl
    , parseModuleFromFile
    , prettyPrintBinder
    , prettyPrintKind
    , prettyPrintString
    , prettyPrintType
    , runAssocList
    , runIdent
    , runModuleName
    , runOpName
    , runProperName
    , showOp
    , showQualified
    , tyFunction
    , tyRecord
    , tydeclIdent
    , tydeclType
    , valdeclBinders
    , valdeclExpression
    , valdeclIdent
    )
import "purescript" Language.PureScript.Label                (runLabel)
import "purescript" Language.PureScript.PSString             (PSString)
import "microlens-platform" Lens.Micro.Platform              (Lens', view)
import "optparse-applicative" Options.Applicative
    ( Parser
    , ParserInfo
    , argument
    , execParser
    , fullDesc
    , header
    , help
    , helper
    , info
    , maybeReader
    , metavar
    , progDesc
    )
import "path" Path
    ( Abs
    , File
    , Path
    , fromAbsFile
    , parseAbsFile
    )

main :: IO ()
main = do
  envArgs <- execParser argsInfo
  let envPrettyPrintConfig =
        PrettyPrintConfig { layoutOptions = defaultLayoutOptions }
  runPurty Env { envArgs, envPrettyPrintConfig } purty

purty :: (HasArgs env, HasPrettyPrintConfig env) => Purty env ()
purty = do
  Args { filePath } <- view argsL
  PrettyPrintConfig { layoutOptions } <- view prettyPrintConfigL
  contents <- liftIO $ readFile (fromAbsFile filePath)
  case parseModuleFromFile identity (fromAbsFile filePath, contents) of
    Left error -> do
      putErrText "Problem parsing module"
      putErrText (show error)
    Right (file, m) -> do
      putText $ "parsed " <> toS file
      liftIO $ renderIO stdout $ layoutSmart layoutOptions (docFromModule m)

docFromModule :: Module -> Doc a
docFromModule (Module _ comments name declarations' exports) =
  foldMap docFromComment comments
    <> docFromModuleExports name exports
    <> line
    <> line
    <> docFromDeclarations imports
    <> line
    <> docFromDeclarations declarations
  where
  (imports, declarations) = span isImportDecl declarations'

docFromModuleExports :: ModuleName -> Maybe [DeclarationRef] -> Doc ann
docFromModuleExports name Nothing =
  "module" <+> pretty (runModuleName name) <+> "where"
docFromModuleExports name (Just exports) =
  "module"
    <+> pretty (runModuleName name)
    <> line
    <> indent 2 (docFromExports exports <> line <> "where")

docFromBinder :: Binder -> Doc a
docFromBinder = pretty . prettyPrintBinder

docFromCaseAlternative :: CaseAlternative -> Doc a
docFromCaseAlternative CaseAlternative {caseAlternativeBinders, caseAlternativeResult} =
  sep (punctuate comma $ map docFromBinder caseAlternativeBinders)
    <+> foldMap docFromGuardedExprCase caseAlternativeResult

docFromComment :: Comment -> Doc a
docFromComment = \case
  BlockComment comment -> enclose "{-" "-}" (pretty comment) <> line
  LineComment comment -> "--" <> pretty comment <> line

docFromConstraint :: Language.PureScript.Constraint -> Doc a
docFromConstraint Constraint { constraintArgs, constraintClass } =
  pretty (showQualified runProperName constraintClass)
    <+> foldMap docFromType constraintArgs

docFromDataConstructors :: [(ProperName 'ConstructorName, [Language.PureScript.Type])] -> Doc a
docFromDataConstructors =
  vsep . zipWith (<+>) ("=" : repeat "|") . map docFromDataConstructor

docFromDataConstructor :: (ProperName 'ConstructorName, [Language.PureScript.Type]) -> Doc a
docFromDataConstructor (name, types) =
  pretty (runProperName name) <+> sep (map (pretty . prettyPrintType) types)

docFromDataType :: DataDeclType -> Doc a
docFromDataType = \case
  Data -> "data"
  Newtype -> "newtype"

docFromDeclarations :: [Declaration] -> Doc a
docFromDeclarations = foldMap docFromDeclaration

docFromDeclaration :: Declaration -> Doc a
docFromDeclaration = \case
  DataBindingGroupDeclaration (declarations) ->
    vsep (toList $ map docFromDeclaration declarations)
  DataDeclaration _ dataType name parameters constructors ->
    docFromDataType dataType
      <+> pretty (runProperName name)
      <+> foldMap docFromParameter parameters
      <> line
      <> indent 2 (docFromDataConstructors constructors)
      <> line
      <> line
  ImportDeclaration _ name importType qualified ->
    "import"
      <+> pretty (runModuleName name)
      <+> docFromImportType importType
      <+> foldMap docFromImportQualified qualified
      <> line
  TypeDeclaration TypeDeclarationData { tydeclIdent, tydeclType } ->
    pretty (runIdent tydeclIdent)
      <+> "::"
      <> line
      <> indent 2 (align $ docFromType tydeclType)
      <> line
  TypeSynonymDeclaration _ name parameters underlyingType ->
    "type"
      <+> pretty (runProperName name)
      <+> foldMap docFromParameter parameters
      <> line
      <> indent 2 ("=" <+> pretty (prettyPrintType underlyingType))
      <> line
  ValueDeclaration ValueDeclarationData { valdeclBinders, valdeclExpression, valdeclIdent } ->
    pretty (runIdent valdeclIdent)
      <+> sep (map docFromBinder valdeclBinders)
      <+> foldMap docFromGuardedExpr valdeclExpression
      <> line
      <> line
  _ -> mempty

docFromDoElement :: DoNotationElement -> Doc a
docFromDoElement = \case
  DoNotationBind binder expr ->
    docFromBinder binder <+> "<-" <+> docFromExpr expr
  DoNotationLet declarations ->
    "let" <> line <> indent 4 (docFromDeclarations declarations)
  DoNotationValue expr -> docFromExpr expr
  PositionedDoNotationElement _ comments element ->
    foldMap docFromComment comments <> docFromDoElement element

docFromExports :: [DeclarationRef] -> Doc a
docFromExports = parentheses . map docFromExport

docFromExport :: DeclarationRef -> Doc a
docFromExport = \case
  KindRef _ name -> "kind" <+> pretty (runProperName name)
  ModuleRef _ name -> "module" <+> pretty (runModuleName name)
  ReExportRef _ _ _ -> mempty
  TypeRef _ name constructors ->
    -- N.B. `Nothing` means everything
    pretty (runProperName name) <> maybe "(..)" docFromConstructors constructors
  TypeClassRef _ name -> "class" <+> pretty (runProperName name)
  TypeInstanceRef _ _ -> mempty
  TypeOpRef _ name -> "type" <+> pretty (showOp name)
  ValueRef _ ident -> pretty (runIdent ident)
  ValueOpRef _ name -> pretty (showOp name)

docFromExpr :: Expr -> Doc a
docFromExpr = \case
  Abs binder expr ->
    "\\" <> docFromBinder binder <+> "->" <> line <> indent 2 (docFromExpr expr)
  Accessor key expr -> docFromExpr expr <> "." <> pretty (prettyPrintString key)
  AnonymousArgument -> "_"
  App expr1 expr2 -> docFromExpr expr1 <+> docFromExpr expr2
  BinaryNoParens op left right ->
    docFromExpr left <+> docFromExpr op <+> docFromExpr right
  Case exprs alternatives ->
    "case"
      <+> sep (punctuate comma $ map docFromExpr exprs)
      <+> "of"
      <> line
      <> indent 2 (vsep $ map docFromCaseAlternative alternatives)
  Constructor name -> pretty (showQualified runProperName name)
  DeferredDictionary _ _ -> mempty
  Do elements ->
    "do"
      <> line
      <> indent 2 (vsep $ map docFromDoElement elements)
  Hole hole -> "?" <> pretty hole
  IfThenElse b t f ->
    "if"
      <+> docFromExpr b
      <+> "then"
      <> line
      <> indent 2 (docFromExpr t)
      <> line
      <> indent (-2) "else"
      <> line
      <> indent 2 (docFromExpr f)
  Let declarations expr ->
    line
      <> indent 2
        ( "let"
        <> line
        <> indent 2 (foldMap docFromDeclaration declarations)
        <> line
        <> "in"
        <> line
        <> indent 2 (docFromExpr expr)
        )
  Literal literal -> docFromLiteral (map docFromExpr literal)
  ObjectUpdate expr obj ->
    docFromExpr expr <+> braces (map (docFromObjectUpdate . map docFromExpr) obj)
  ObjectUpdateNested expr pathTree ->
    docFromExpr expr <+> docFromPathTree pathTree
  Op op -> pretty (showQualified runOpName op)
  Parens expr -> parens (docFromExpr expr)
  PositionedValue _ comments expr ->
    foldMap docFromComment comments <> docFromExpr expr
  TypeClassDictionary _ _ _ -> mempty
  TypeClassDictionaryAccessor _ _ -> mempty
  TypeClassDictionaryConstructorApp _ _ -> mempty
  TypedValue _ expr exprType ->
    docFromExpr expr <+> "::" <+> pretty (prettyPrintType exprType)
  UnaryMinus expr -> "-" <> docFromExpr expr
  Var ident -> pretty (showQualified runIdent ident)

docFromGuard :: Guard -> Doc a
docFromGuard = \case
  ConditionGuard expr -> docFromExpr expr
  PatternGuard binder expr -> docFromBinder binder <+> "<-" <+> docFromExpr expr

docFromGuardedExpr :: GuardedExpr -> Doc a
docFromGuardedExpr = docFromGuardedExpr' "="

docFromGuardedExprCase :: GuardedExpr -> Doc a
docFromGuardedExprCase = docFromGuardedExpr' "->"

docFromGuardedExpr' :: Doc a -> GuardedExpr -> Doc a
docFromGuardedExpr' separator (GuardedExpr [] expr) =
  separator <+> docFromExpr expr
docFromGuardedExpr' separator (GuardedExpr guards expr) =
  line <> indent 2 guardedExpr
  where
  guardedExpr =
    "|"
      <+> sep (punctuate comma (map docFromGuard guards))
      <+> separator
      <> line
      <> indent 2 (docFromExpr expr)

docFromConstructors :: [ProperName 'ConstructorName] -> Doc a
docFromConstructors [] = mempty
docFromConstructors constructors =
  parentheses $ map docFromConstructor constructors

docFromConstructor :: ProperName 'ConstructorName -> Doc a
docFromConstructor = pretty . runProperName

docFromKind :: Kind -> Doc a
docFromKind = pretty . prettyPrintKind

docFromLiteral :: Literal (Doc a) -> Doc a
docFromLiteral = \case
  ArrayLiteral xs -> brackets xs
  BooleanLiteral b -> pretty b
  CharLiteral c -> pretty c
  NumericLiteral (Left x) -> pretty x
  NumericLiteral (Right x) -> pretty x
  ObjectLiteral obj -> braces (map docFromObject obj)
  StringLiteral str -> pretty (prettyPrintString str)

docFromImportQualified :: ModuleName -> Doc a
docFromImportQualified name = "as" <+> pretty (runModuleName name)

docFromImportType :: ImportDeclarationType -> Doc a
docFromImportType = \case
  Explicit declarationRefs -> line <> indent 2 (docFromExports declarationRefs)
  Hiding declarationRefs ->
    line <> indent 2 ("hiding" <+> docFromExports declarationRefs)
  Implicit -> mempty

docFromObject :: (PSString, Doc a) -> Doc a
docFromObject (key, val) = pretty (prettyPrintString key) <> ":" <+> val

docFromObjectUpdate :: (PSString, Doc a) -> Doc a
docFromObjectUpdate (key, val) = pretty (prettyPrintString key) <+> "=" <+> val

docFromParameter :: (Text, Maybe Kind) -> Doc a
docFromParameter (parameter, Nothing) = pretty parameter
docFromParameter (parameter, Just k) =
  parens (pretty parameter <+> "::" <+> docFromKind k)

docFromPathTree :: PathTree Expr -> Doc a
docFromPathTree (PathTree paths) =
  braces (map docFromPathNode $ runAssocList paths)

docFromPathNode :: (PSString, PathNode Expr) -> Doc a
docFromPathNode (key, Leaf expr) =
  pretty (prettyPrintString key) <+> "=" <+> docFromExpr expr
docFromPathNode (key, Branch path) =
  pretty (prettyPrintString key) <+> docFromPathTree path

docFromType :: Language.PureScript.Type -> Doc a
docFromType =
  go . everywhereOnTypesTopDown (convertForAlls []) . everywhereOnTypes convertTypeApps
  where
  go = \case
    BinaryNoParensType op left right ->
      docFromType left <+> docFromType op <+> docFromType right
    ConstrainedType constraint type' ->
      docFromConstraint constraint <+> "=>" <> line <> docFromType type'
    ForAll var type' _ ->
      "forall" <+> pretty var <> "." <> line <> docFromType type'
    KindedType type' kind ->
      parens (docFromType type' <+> "::" <+> docFromKind kind)
    ParensInType type' -> parens (docFromType type')
    PrettyPrintForAll [] type' -> docFromType type'
    PrettyPrintForAll vars type' ->
      "forall"
        <+> hcat (punctuate space $ map pretty vars)
        <> "."
        <> line
        <> docFromType type'
    PrettyPrintFunction f x -> docFromType f <+> "->" <> line <> docFromType x
    type'@PrettyPrintObject {} -> "{" <> hsep (convertRow [] type') <> "}"
    type'@RCons {} -> "(" <> hsep (convertRow [] type') <> ")"
    REmpty -> "()"
    Skolem _ _ _ _ -> mempty
    TUnknown _ -> mempty
    TypeApp f x -> docFromType f <+> docFromType x
    TypeConstructor constructor ->
      pretty (showQualified runProperName constructor)
    TypeLevelString str -> pretty (prettyPrintString str)
    TypeOp op -> pretty (showQualified runOpName op)
    TypeVar var -> pretty var
    TypeWildcard _ -> "_"
  convertRow rest = \case
    RCons label type' tail@REmpty ->
      convertRow
        ( pretty (prettyPrintString $ runLabel label)
        <+> "::"
        <+> docFromType type'
        : rest
        )
        tail
    RCons label type' tail@RCons{} ->
      convertRow
        ( pretty (prettyPrintString $ runLabel label)
        <+> "::"
        <+> docFromType type'
        <> ","
        : rest
        )
        tail
    RCons label type' tail ->
      convertRow
        ( pretty (prettyPrintString $ runLabel label)
        <+> "::"
        <+> docFromType type'
        <+> "|"
        : rest
        )
        tail
    REmpty -> reverse rest
    x -> reverse (docFromType x : rest)
  convertTypeApps = \case
    TypeApp (TypeApp f g) x | f == tyFunction -> PrettyPrintFunction g x
    TypeApp o r | o == tyRecord -> PrettyPrintObject r
    x -> x
  convertForAlls vars = \case
    ForAll var (quantifiedType@ForAll {}) _ ->
      convertForAlls (var : vars) quantifiedType
    ForAll var quantifiedType _ -> PrettyPrintForAll (var : vars) quantifiedType
    other -> other

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

parentheses :: [Doc a] -> Doc a
parentheses = enclosedWith "(" ")"

data Args
  = Args
    { filePath :: !(Path Abs File)
    }

class HasArgs env where
  argsL :: Lens' env Args

args :: Parser Args
args =
  Args
    <$> argument
      (maybeReader parseAbsFile)
      ( help "PureScript file to pretty print"
      <> metavar "FILE"
      )

argsInfo :: ParserInfo Args
argsInfo =
  info
    (helper <*> args)
    ( fullDesc
    <> progDesc "Pretty print a PureScript file"
    <> header "purty - A PureScript pretty-printer"
    )

data PrettyPrintConfig
  = PrettyPrintConfig
    { layoutOptions :: !LayoutOptions
    }

class HasPrettyPrintConfig env where
  prettyPrintConfigL :: Lens' env PrettyPrintConfig

data Env
  = Env
    { envArgs              :: !Args
    , envPrettyPrintConfig :: !PrettyPrintConfig
    }

class HasEnv env where
  envL :: Lens' env Env

instance HasArgs Env where
  argsL f env = (\envArgs -> env { envArgs }) <$> f (envArgs env)

instance HasEnv Env where
  envL = identity

instance HasPrettyPrintConfig Env where
  prettyPrintConfigL f env = (\envPrettyPrintConfig -> env { envPrettyPrintConfig }) <$> f (envPrettyPrintConfig env)

-- Locally defined rio since dependencies are wild.
newtype Purty r a
  = Purty { unPurty :: ReaderT r IO a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader r
    )

runPurty :: r -> Purty r a -> IO a
runPurty r (Purty x) = runReaderT x r
