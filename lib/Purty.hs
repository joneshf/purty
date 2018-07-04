module Purty where

import "rio" RIO hiding (ask)

import "freer-simple" Control.Monad.Freer                    (Eff, Members)
import "freer-simple" Control.Monad.Freer.Error              (Error)
import "freer-simple" Control.Monad.Freer.Reader             (Reader, ask)
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


import qualified "this" Doc.Dynamic
import qualified "this" Doc.Static
import qualified "this" File
import qualified "this" Log
import qualified "this" Module
import qualified "this" Output

fromAbsFile ::
  ( Members
    '[ Error ParseError
     , File.File
     , Log.Log
     , Reader Formatting
     , Reader LayoutOptions
     ]
    e
  ) =>
  Path Abs File ->
  Eff e (SimpleDocStream a)
fromAbsFile filePath = do
  formatting <- ask
  Log.debug ("Formatting: " <> display formatting)
  layoutOptions <- ask
  Log.debug ("LayoutOptions: " <> displayShow layoutOptions)
  m <- Module.parse filePath
  Log.debug "Parsed module:"
  Log.debug (displayShow m)
  case formatting of
    Dynamic -> pure (layoutSmart layoutOptions $ Doc.Dynamic.fromModule m)
    Static  -> pure (layoutSmart layoutOptions $ Doc.Static.fromModule m)

fromPurtyFilePath ::
  ( Members
    '[ Error ParseError
     , File.File
     , Log.Log
     , Output.Output
     , Reader Formatting
     , Reader LayoutOptions
     , Reader Output
     ]
    e
  ) =>
  PurtyFilePath ->
  Eff e ()
fromPurtyFilePath filePath = do
  output <- ask
  Log.debug ("Output: " <> display output)
  Log.debug ("Converting " <> display filePath <> " to an absolute path")
  absPath <- File.absolute filePath
  Log.debug ("Converted file to absolute: " <> displayShow absPath)
  Log.debug "Running main `purty` program"
  stream <- Purty.fromAbsFile absPath
  Log.debug "Successfully created stream for rendering"
  Log.debug (displayShow $ void stream)
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
    '[ Reader Formatting
     , Reader LayoutOptions
     , Reader Output
     , Error ParseError
     , File.File
     , Log.Log
     , Output.Output
     , IO
     ]
    ()
program = \case
  Args { filePath } -> Purty.fromPurtyFilePath filePath
  Defaults -> liftIO (putDoc $ prettyExpr $ embed inject defaultConfig)
