module Purty where

import "rio" RIO hiding (ask, withSystemTempFile)

import "freer-simple" Control.Monad.Freer
    ( Eff
    , LastMember
    , Members
    )
import "freer-simple" Control.Monad.Freer.Error              (Error, throwError)
import "freer-simple" Control.Monad.Freer.Reader             (Reader, ask)
import "freer-simple" Data.OpenUnion                         ((:++:))
import "prettyprinter" Data.Text.Prettyprint.Doc
    ( LayoutOptions
    , SimpleDocStream
    , layoutSmart
    )
import "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text (putDoc, renderIO)
import "dhall" Dhall                                         (embed, inject)
import "dhall" Dhall.Pretty                                  (prettyExpr)
import "purescript" Language.PureScript
    ( parseModuleFromFile
    )
import "path" Path                                           (Abs, File, Path)
import "path-io" Path.IO
    ( copyFile
    , copyPermissions
    , withSystemTempFile
    )
import "parsec" Text.Parsec                                  (ParseError)

import "this" Args (Args(Args, Defaults, filePath))
import "this" Env
    ( Formatting(Dynamic, Static)
    , Output(InPlace, StdOut)
    , PurtyFilePath
    , absolutize
    , defaultConfig
    )

import qualified "path" Path

import qualified "this" Annotation
import qualified "this" Declaration
import qualified "this" Exit
import qualified "this" Export
import qualified "this" Log
import qualified "this" Module
import qualified "this" Name

fromAbsFile ::
  ( LastMember IO e
  , Members
    ( Declaration.Errors
    :++: Export.Errors
    :++: Name.Errors
    :++: '[ Error ParseError
          , Log.Log
          , Reader Formatting
          , Reader LayoutOptions
          ]
    )
    e
  ) =>
  Path Abs File ->
  Eff e (SimpleDocStream Annotation.Sorted)
fromAbsFile filePath = do
  formatting <- ask
  Log.debug ("Formatting: " <> display formatting)
  layoutOptions <- ask
  Log.debug ("LayoutOptions: " <> displayShow layoutOptions)
  contents <- readFileUtf8 (Path.fromAbsFile filePath)
  Log.debug "Read file contents:"
  Log.debug (display contents)
  (_, m) <- either throwError pure (parseModuleFromFile id (Path.fromAbsFile filePath, contents))
  Log.debug "Parsed module:"
  Log.debug (displayShow m)
  ast <- Module.fromPureScript m
  Log.debug "Converted AST:"
  Log.debug (display ast)
  let sorted = Module.sortImports (Module.sortExports ast)
      normalized = Module.normalize sorted
      doc = case formatting of
        Dynamic -> Module.dynamic normalized
        Static  -> Module.static normalized
      stream = layoutSmart layoutOptions doc
  Log.debug "Sorted AST:"
  Log.debug (display sorted)
  Log.debug "Normalized AST:"
  Log.debug (display normalized)
  Log.debug "Doc:"
  Log.debug (displayShow doc)
  Log.debug "Stream:"
  Log.debug (displayShow stream)
  pure stream

fromPurtyFilePath ::
  ( LastMember IO e
  , Members
    ( Declaration.Errors
    :++: Export.Errors
    :++: Name.Errors
    :++: '[ Error ParseError
          , Log.Log
          , Reader Formatting
          , Reader LayoutOptions
          , Reader Output
          ]
    )
    e
  ) =>
  PurtyFilePath ->
  Eff e ()
fromPurtyFilePath filePath = do
  output <- ask
  Log.debug ("Output: " <> display output)
  Log.debug ("Converting " <> display filePath <> " to an absolute path")
  absPath <- absolutize filePath
  Log.debug ("Converted file to absolute: " <> displayShow absPath)
  Log.debug "Running main `purty` program"
  stream <- Purty.fromAbsFile absPath
  Log.debug "Successfully created stream for rendering"
  Log.debug (displayShow $ void stream)
  case output of
    InPlace -> liftIO $ withSystemTempFile "purty.purs" $ \fp h -> do
      renderIO h stream
      hClose h
      copyPermissions absPath fp
      copyFile fp absPath
    StdOut -> do
      Log.debug "Printing to stdout"
      liftIO $ renderIO stdout stream

program ::
  Args ->
  Eff
    ( Declaration.Errors
    :++: Export.Errors
    :++: Name.Errors
    :++: '[ Error ParseError
          , Exit.Exit
          , Log.Log
          , Reader Formatting
          , Reader LayoutOptions
          , Reader Output
          , IO
          ]
    )
    ()
program = \case
  Args { filePath } -> Purty.fromPurtyFilePath filePath
  Defaults -> liftIO (putDoc $ prettyExpr $ embed inject defaultConfig)
