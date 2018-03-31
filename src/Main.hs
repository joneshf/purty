module Main where

import "protolude" Protolude

import "prettyprinter" Data.Text.Prettyprint.Doc
    ( Doc
    , LayoutOptions
    , defaultLayoutOptions
    , enclose
    , layoutSmart
    , line
    , pretty
    , tupled
    , (<+>)
    )
import "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text (renderIO)
import "purescript" Language.PureScript
    ( Comment(BlockComment, LineComment)
    , DeclarationRef(KindRef, ModuleRef, ReExportRef, TypeClassRef, TypeInstanceRef, TypeOpRef, TypeRef, ValueOpRef, ValueRef)
    , Module(Module)
    , ProperName
    , ProperNameType(ConstructorName)
    , parseModuleFromFile
    , runIdent
    , runModuleName
    , runProperName
    , showOp
    )
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
  runApp Env { envArgs, envPrettyPrintConfig } purty

purty :: (HasArgs env, HasPrettyPrintConfig env) => App env ()
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
docFromModule (Module _ comments name _declarations exports) =
  foldMap docFromComment comments
    <> "module"
    <+> pretty (runModuleName name)
    <+> foldMap docFromExports exports
    <+> "where"
    <> line

docFromComment :: Comment -> Doc a
docFromComment = \case
  BlockComment comment -> enclose "{-" "-}" (pretty comment) <> line
  LineComment comment -> "--" <> pretty comment <> line

docFromExports :: [DeclarationRef] -> Doc a
docFromExports = tupled . map docFromExport

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

docFromConstructors :: [ProperName 'ConstructorName] -> Doc a
docFromConstructors [] = mempty
docFromConstructors constructors =
  tupled $ map (pretty . runProperName) constructors

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
newtype App r a
  = App { unApp :: ReaderT r IO a }
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader r
    )

runApp :: r -> App r a -> IO a
runApp r (App x) = runReaderT x r
