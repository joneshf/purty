module Purty where

import "rio" RIO

import "lens" Control.Monad.Error.Lens           (throwing)
import "mtl" Control.Monad.Except
    ( ExceptT
    , MonadError
    , runExceptT
    )
import "transformers" Control.Monad.Trans.Except (catchE)
import "prettyprinter" Data.Text.Prettyprint.Doc (SimpleDocStream, layoutSmart)
import "purescript" Language.PureScript          (parseModuleFromFile)
import "path" Path                               (Abs, File, Path, fromAbsFile)

import "this" Env
    ( Formatting(Dynamic, Static)
    , HasFormatting(formattingL)
    , HasLayoutOptions(layoutOptionsL)
    )
import "this" Error (IsParseError(_ParseError))

import qualified "this" Purty.AST
import qualified "this" Purty.Doc.Dynamic
import qualified "this" Purty.Doc.Static

purty ::
  ( HasFormatting env
  , HasLayoutOptions env
  , HasLogFunc env
  , Purty.AST.IsMissingName error
  , IsParseError error
  ) =>
  Path Abs File ->
  Purty env error (SimpleDocStream a)
purty filePath = do
  formatting <- view formattingL
  layoutOptions <- view layoutOptionsL
  contents <- readFileUtf8 (fromAbsFile filePath)
  logDebug "Read file contents:"
  logDebug (display contents)
  (_, m) <- either (throwing _ParseError) pure (parseModuleFromFile id (fromAbsFile filePath, contents))
  logDebug "Parsed module:"
  logDebug (displayShow m)
  ast <- Purty.AST.fromPureScript m
  logDebug "Converted AST:"
  logDebug (display ast)
  case formatting of
    Dynamic -> pure (layoutSmart layoutOptions $ Purty.Doc.Dynamic.fromModule ast)
    Static  -> pure (layoutSmart layoutOptions $ Purty.Doc.Static.fromModule ast)

newtype Purty env error c
  = Purty (ReaderT env (ExceptT error IO) c)
  deriving
    ( Applicative
    , Functor
    , Monad
    , MonadError error
    , MonadIO
    , MonadReader env
    )

handle :: Purty a b c -> (b -> Purty a d c) -> Purty a d c
handle (Purty (ReaderT f)) g = Purty $ ReaderT $ \env ->
  catchE (f env) $ \y -> case g y of
    Purty h -> runReaderT h env

run :: a -> Purty a Void b -> IO b
run env (Purty f) = do
  result <- runExceptT (runReaderT f env)
  either absurd pure result
