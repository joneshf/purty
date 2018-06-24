module Error where

import "rio" RIO

import "lens" Control.Lens  (Prism', prism)
import "base" System.Exit   (exitFailure)
import "parsec" Text.Parsec (ParseError)

import qualified "this" AST
import qualified "this" Name

data Error
  = AST !AST.Error
  | Name !Name.Error
  | NotImplemented !AST.NotImplemented
  | Parse !ParseError

instance AST.IsEmptyExplicitExports Error where
  _EmptyExplicitExports = AST._Error.AST._EmptyExplicitExports

instance AST.IsInstanceExported Error where
  _InstanceExported = AST._Error.AST._InstanceExported

instance AST.IsInvalidExport Error where
  _InvalidExport = AST._Error.AST._InvalidExport

instance AST.IsReExportExported Error where
  _ReExportExported = AST._Error.AST._ReExportExported

instance AST.IsError Error where
  _Error = prism AST $ \case
    AST x -> Right x
    x -> Left x

instance AST.IsNotImplemented Error where
  _NotImplemented = notImplemented.AST._NotImplemented
    where
    notImplemented = prism NotImplemented $ \case
      NotImplemented x -> Right x
      x -> Left x

instance Name.IsError Error where
  _Error = prism Name $ \case
    Name x -> Right x
    x -> Left x

instance Name.IsMissing Error where
  _Missing = Name._Error . Name._Missing

class
  ( AST.IsError error
  , AST.IsNotImplemented error
  , IsParseError error
  ) =>
  IsError error where
    _Error :: Prism' error Error

instance IsError Error where
  _Error = prism id Right

class IsParseError error where
  _ParseError :: Prism' error ParseError

instance IsParseError ParseError where
  _ParseError = prism id Right

instance IsParseError Error where
  _ParseError = prism Parse $ \case
    Parse x -> Right x
    x -> Left x

errors :: (HasLogFunc env, MonadIO f, MonadReader env f) => Error -> f a
errors = \case
  AST err -> do
    logError "Problem converting to our AST"
    logError (display err)
    liftIO exitFailure
  Name err -> do
    logError "Problem converting a name"
    logError (display err)
    liftIO exitFailure
  NotImplemented err -> do
    logError (display err)
    logError "Report this to https://gitlab.com/joneshf/purty"
    liftIO exitFailure
  Parse err -> do
    logError "Problem parsing module"
    logError (displayShow err)
    liftIO exitFailure
