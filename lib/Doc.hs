-- N.B. The only reason we have an explicit export list is to get additional
-- analysis on the module, like unused binds.
-- Don't mis-interpret the export list as saying what the private/public API is.
-- If you need something from this module, export it.
module Doc
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
  , fromParameters
  , fromBool
  , partitionImports
  , valueDeclarationFromAnonymousDeclaration
  ) where

import "rio" RIO

import "containers" Data.IntMap.Strict           (findWithDefault)
import "prettyprinter" Data.Text.Prettyprint.Doc
    ( Doc
    , comma
    , enclose
    , hsep
    , line
    , parens
    , pretty
    , punctuate
    , space
    , (<+>)
    )
import "purescript" Language.PureScript
    ( Comment(BlockComment, LineComment)
    , DataDeclType(Data, Newtype)
    , Declaration(ImportDeclaration)
    , DeclarationRef(TypeRef)
    , Expr
    , Fixity(Fixity)
    , FunctionalDependency(FunctionalDependency, fdDetermined, fdDeterminers)
    , GuardedExpr(GuardedExpr)
    , Ident
    , ImportDeclarationType(Explicit, Implicit)
    , Kind
    , ModuleName
    , NameKind
    , ProperName
    , ProperNameType(ConstructorName)
    , SourceAnn
    , Type(ForAll, PrettyPrintForAll, PrettyPrintFunction, PrettyPrintObject, TypeApp)
    , ValueDeclarationData(ValueDeclarationData, valdeclBinders, valdeclExpression, valdeclIdent, valdeclName, valdeclSourceAnn)
    , compDecRef
    , prettyPrintKind
    , prettyPrintObjectKey
    , runModuleName
    , runProperName
    , showAssoc
    , tyFunction
    , tyRecord
    )
import "purescript" Language.PureScript.PSString (PSString)
import "rio" RIO.List                            (sort, sortBy, sortOn)

convertForAlls :: [Text] -> Language.PureScript.Type -> Language.PureScript.Type
convertForAlls vars = \case
  ForAll var (quantifiedType@ForAll {}) _ ->
    convertForAlls (var : vars) quantifiedType
  ForAll var quantifiedType _ -> PrettyPrintForAll (var : vars) quantifiedType
  other -> other

convertTypeApps :: Language.PureScript.Type -> Language.PureScript.Type
convertTypeApps = \case
  TypeApp (TypeApp f g) x | f == tyFunction -> PrettyPrintFunction g x
  TypeApp o r | o == tyRecord -> PrettyPrintObject r
  x -> x

fromComment :: Comment -> Doc a
fromComment = \case
  BlockComment comment -> enclose "{-" "-}" (pretty comment) <> line
  LineComment comment -> "--" <> pretty comment <> line

fromComments :: (Foldable f) => f Comment -> Doc a
fromComments = foldMap fromComment

fromConstructor :: ProperName 'ConstructorName -> Doc a
fromConstructor = pretty . runProperName

fromConstructors :: [ProperName 'ConstructorName] -> Doc a
fromConstructors [] = mempty
fromConstructors constructors =
  "(" <> hsep (punctuate comma $ fmap fromConstructor constructors) <> ")"

fromDataType :: DataDeclType -> Doc a
fromDataType = \case
  Data -> "data"
  Newtype -> "newtype"

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

fromImportQualified :: ModuleName -> Doc a
fromImportQualified name = space <> "as" <+> pretty (runModuleName name)

fromKind :: Kind -> Doc a
fromKind = pretty . prettyPrintKind

fromObjectUpdate :: (PSString, Doc a) -> Doc a
fromObjectUpdate (key, val) = fromPSString key <+> "=" <+> val

fromParameter :: (Text, Maybe Kind) -> Doc a
fromParameter (parameter, Nothing) = pretty parameter
fromParameter (parameter, Just k) =
  parens (pretty parameter <+> "::" <+> fromKind k)

fromParameters :: [(Text, Maybe Kind)] -> Doc a
fromParameters = \case
  [] -> mempty
  parameters -> space <> hsep (fmap fromParameter parameters)

fromBool :: Bool -> Doc a
fromBool b = if b then "true" else "false"

fromPSString :: PSString -> Doc a
fromPSString = pretty . prettyPrintObjectKey

partitionImports ::
  [Declaration] ->
  ([Declaration], [Declaration], [Declaration])
partitionImports = go ([], [], [])
  where
  go (open, explicit, qualified) = \case
    [] -> (sortImports open, sortImports explicit, sortImports qualified)
    i@(ImportDeclaration _ _ _ (Just _)) : rest ->
      go (open, explicit, i : qualified) rest
    i@(ImportDeclaration _ _ Implicit _) : rest ->
      go (i : open, explicit, qualified) rest
    i@ImportDeclaration {} : rest ->
      go (open, i : explicit, qualified) rest
    _ : rest -> go (open, explicit, qualified) rest

sortImports :: [Declaration] -> [Declaration]
sortImports = mapMaybe sortExplicits . sortOn importName
  where
  importName :: Declaration -> Maybe ModuleName
  importName = \case
    ImportDeclaration _ name _ _ -> Just name
    _ -> Nothing
  sortConstructors :: DeclarationRef -> DeclarationRef
  sortConstructors = \case
    TypeRef s name constructors -> TypeRef s name (fmap sort constructors)
    r -> r
  sortExplicits :: Declaration -> Maybe Declaration
  sortExplicits = \case
    ImportDeclaration s name (Explicit refs) moduleName ->
      Just (ImportDeclaration s name (Explicit (sortConstructors <$> sortBy compDecRef refs)) moduleName)
    i@ImportDeclaration {} -> Just i
    _ -> Nothing

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
