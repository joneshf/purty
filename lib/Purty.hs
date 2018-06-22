module Purty where

import "rio" RIO

import "lens" Control.Monad.Error.Lens           (throwing)
import "prettyprinter" Data.Text.Prettyprint.Doc (SimpleDocStream, layoutSmart)
import "purescript" Language.PureScript          (parseModuleFromFile)
import "path" Path                               (Abs, File, Path)

import "this" Env
    ( Formatting(Dynamic, Static)
    , HasFormatting(formattingL)
    , HasLayoutOptions(layoutOptionsL)
    )
import "this" Error (IsParseError(_ParseError))
import "this" App (App)

import qualified "path" Path

import qualified "this" Purty.AST
import qualified "this" Purty.Doc.Dynamic
import qualified "this" Purty.Doc.Static

fromAbsFile ::
  ( HasFormatting env
  , HasLayoutOptions env
  , HasLogFunc env
  , Purty.AST.IsMissingName error
  , IsParseError error
  ) =>
  Path Abs File ->
  App env error (SimpleDocStream a)
fromAbsFile filePath = do
  formatting <- view formattingL
  layoutOptions <- view layoutOptionsL
  contents <- readFileUtf8 (Path.fromAbsFile filePath)
  logDebug "Read file contents:"
  logDebug (display contents)
  (_, m) <- either (throwing _ParseError) pure (parseModuleFromFile id (Path.fromAbsFile filePath, contents))
  logDebug "Parsed module:"
  logDebug (displayShow m)
  ast <- Purty.AST.fromPureScript m
  logDebug "Converted AST:"
  logDebug (display ast)
  case formatting of
    Dynamic -> pure (layoutSmart layoutOptions $ Purty.Doc.Dynamic.fromModule ast)
    Static  -> pure (layoutSmart layoutOptions $ Purty.Doc.Static.fromModule ast)
