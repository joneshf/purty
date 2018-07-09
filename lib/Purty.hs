module Purty where

import "rio" RIO hiding (ask)

import "freer-simple" Control.Monad.Freer                    (Eff, Members)
import "freer-simple" Control.Monad.Freer.Error              (Error)
import "freer-simple" Control.Monad.Freer.Reader             (Reader, ask)
import "freer-simple" Data.OpenUnion                         ((:++:))
import "prettyprinter" Data.Text.Prettyprint.Doc
    ( LayoutOptions
    , SimpleDocStream
    , layoutSmart
    )
import "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text (putDoc)
import "dhall" Dhall                                         (embed, inject)
import "dhall" Dhall.Pretty                                  (prettyExpr)
import "path" Path                                           (Abs, File, Path)
import "parsec" Text.Parsec                                  (ParseError)

import "this" Args (Args(Args, Defaults, filePath))
import "this" Env
    ( Formatting(Dynamic, Static)
    , Output(InPlace, StdOut)
    , PurtyFilePath
    , defaultConfig
    )

import qualified "this" Annotation
import qualified "this" Declaration.Class
import qualified "this" Declaration.DataType
import qualified "this" Declaration.Fixity
import qualified "this" Declaration.Value
import qualified "this" Exit
import qualified "this" Export
import qualified "this" File
import qualified "this" Kind
import qualified "this" Log
import qualified "this" Module
import qualified "this" Name
import qualified "this" Output
import qualified "this" Type

fromAbsFile ::
  ( Members
    ( Declaration.Class.Errors
    :++: Declaration.DataType.Errors
    :++: Declaration.Fixity.Errors
    :++: Declaration.Value.Errors
    :++: Export.Errors
    :++: Kind.Errors
    :++: Name.Errors
    :++: Type.Errors
    :++: '[ Error ParseError
          , File.File
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
  Log.debug "Formatting:"
  Log.inspect formatting
  layoutOptions <- ask
  Log.debug "LayoutOptions:"
  Log.inspect layoutOptions
  m <- Module.parse filePath
  Log.debug "Parsed module:"
  Log.inspect m
  ast <- Module.fromPureScript m
  Log.debug "Converted AST:"
  Log.inspect ast
  let sorted = Module.sortImports (Module.sortExports ast)
      normalized = Module.normalize sorted
      doc = case formatting of
        Dynamic -> Module.dynamic normalized
        Static  -> Module.static normalized
      stream = layoutSmart layoutOptions doc
  Log.debug "Sorted AST:"
  Log.inspect sorted
  Log.debug "Normalized AST:"
  Log.inspect normalized
  Log.debug "Doc:"
  Log.inspect doc
  Log.debug "Stream:"
  Log.inspect stream
  pure stream

fromPurtyFilePath ::
  ( Members
    ( Declaration.Class.Errors
    :++: Declaration.DataType.Errors
    :++: Declaration.Fixity.Errors
    :++: Declaration.Value.Errors
    :++: Export.Errors
    :++: Kind.Errors
    :++: Name.Errors
    :++: Type.Errors
    :++: '[ Error ParseError
          , File.File
          , Log.Log
          , Output.Output
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
  Log.debug "Output:"
  Log.inspect output
  Log.debug "Converting the following file path to an absolute path:"
  Log.inspect filePath
  absPath <- File.absolute filePath
  Log.debug "Converted file to absolute:"
  Log.inspect absPath
  Log.debug "Running main `purty` program"
  stream <- Purty.fromAbsFile absPath
  Log.debug "Successfully created stream for rendering"
  Log.inspect (void stream)
  case output of
    InPlace -> do
      Log.debug "Replacing file"
      Output.inPlace absPath stream
    StdOut -> do
      Log.debug "Printing to stdout"
      Output.stdOut stream

program ::
  Args ->
  Eff
    ( Declaration.Class.Errors
    :++: Declaration.DataType.Errors
    :++: Declaration.Fixity.Errors
    :++: Declaration.Value.Errors
    :++: Export.Errors
    :++: Kind.Errors
    :++: Name.Errors
    :++: Type.Errors
    :++: '[ Error ParseError
          , Exit.Exit
          , File.File
          , Log.Log
          , Output.Output
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
