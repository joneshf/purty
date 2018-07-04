module Purty where

import "rio" RIO hiding (ask, withSystemTempFile)

import "freer-simple" Control.Monad.Freer
    ( Eff
    , LastMember
    , Members
    )
import "freer-simple" Control.Monad.Freer.Error              (Error, throwError)
import "freer-simple" Control.Monad.Freer.Reader             (Reader, ask)
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
    , defaultConfig
    )

import qualified "path" Path

import qualified "this" Doc.Dynamic
import qualified "this" Doc.Static
import qualified "this" File
import qualified "this" Log

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
  contents <- File.read filePath
  Log.debug "Read file contents:"
  Log.debug (display contents)
  (_, m) <- either throwError pure (parseModuleFromFile id (Path.fromAbsFile filePath, contents))
  Log.debug "Parsed module:"
  Log.debug (displayShow m)
  case formatting of
    Dynamic -> pure (layoutSmart layoutOptions $ Doc.Dynamic.fromModule m)
    Static  -> pure (layoutSmart layoutOptions $ Doc.Static.fromModule m)

fromPurtyFilePath ::
  ( LastMember IO e
  , Members
    '[ Error ParseError
     , File.File
     , Log.Log
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
    '[ Reader Formatting
     , Reader LayoutOptions
     , Reader Output
     , Error ParseError
     , File.File
     , Log.Log
     , IO
     ]
    ()
program = \case
  Args { filePath } -> Purty.fromPurtyFilePath filePath
  Defaults -> liftIO (putDoc $ prettyExpr $ embed inject defaultConfig)
