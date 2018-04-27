-- N.B. The only reason we have an explicit export list is to get additional
-- analysis on the module, like unused binds.
-- Don't mis-interpret the export list as saying what the private/public API is.
-- If you need something from this module, export it.
module Doc
  ( convertForAlls
  , convertTypeApps
  , fromBinder
  , fromBinders
  , fromComments
  , fromConstructors
  , fromDataType
  , fromFixity
  , fromFunctionalDependencies
  , fromImportQualified
  , fromKind
  , fromObject
  , fromObjectUpdate
  , fromPSString
  , fromParameters
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
    ( Binder
    , Comment(BlockComment, LineComment)
    , DataDeclType(Data, Newtype)
    , Expr
    , Fixity(Fixity)
    , FunctionalDependency(FunctionalDependency, fdDetermined, fdDeterminers)
    , GuardedExpr(GuardedExpr)
    , Ident
    , Kind
    , ModuleName
    , NameKind
    , ProperName
    , ProperNameType(ConstructorName)
    , SourceAnn
    , Type(ForAll, PrettyPrintForAll, PrettyPrintFunction, PrettyPrintObject, TypeApp)
    , ValueDeclarationData(ValueDeclarationData, valdeclBinders, valdeclExpression, valdeclIdent, valdeclName, valdeclSourceAnn)
    , prettyPrintBinder
    , prettyPrintKind
    , prettyPrintString
    , runModuleName
    , runProperName
    , showAssoc
    , tyFunction
    , tyRecord
    )
import "purescript" Language.PureScript.PSString (PSString)
import "rio" RIO.Text                            (dropAround)

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

fromBinder :: Binder -> Doc a
fromBinder = pretty . prettyPrintBinder

fromBinders :: [Binder] -> Doc a
fromBinders = \case
  [] -> mempty
  binders -> space <> hsep (fmap fromBinder binders)

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

fromObject :: (PSString, Doc a) -> Doc a
fromObject (key, val) = fromPSString key <> ":" <+> val

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

fromPSString :: PSString -> Doc a
fromPSString = pretty . dropAround (== '"') . prettyPrintString

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
