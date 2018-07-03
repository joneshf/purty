module Purty where

import "rio" RIO hiding (withSystemTempFile)

import "lens" Control.Monad.Error.Lens                       (throwing)
import "prettyprinter" Data.Text.Prettyprint.Doc
    ( SimpleDocStream
    , layoutSmart
    )
import "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text (renderIO)
import "purescript" Language.PureScript
    ( parseModuleFromFile
    )
import "path" Path                                           (Abs, File, Path)
import "path-io" Path.IO
    ( copyFile
    , copyPermissions
    , withSystemTempFile
    )

import "this" App   (App)
import "this" Env
    ( Formatting(Dynamic, Static)
    , HasEnv(envL)
    , HasFormatting(formattingL)
    , HasLayoutOptions(layoutOptionsL)
    , HasOutput(outputL)
    , Output(InPlace, StdOut)
    , PurtyFilePath
    , absolutize
    )
import "this" Error (IsParseError(_ParseError))

import qualified "path" Path

import qualified "this" Doc.Dynamic
import qualified "this" Doc.Static

fromAbsFile ::
  ( HasFormatting env
  , HasLayoutOptions env
  , HasLogFunc env
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
  case formatting of
    Dynamic -> pure (layoutSmart layoutOptions $ Doc.Dynamic.fromModule m)
    Static  -> pure (layoutSmart layoutOptions $ Doc.Static.fromModule m)

fromPurtyFilePath ::
  ( HasEnv env
  , IsParseError error
  ) =>
  PurtyFilePath ->
  App env error ()
fromPurtyFilePath filePath = do
  env <- view envL
  output <- view outputL
  logDebug ("Env: " <> display env)
  logDebug ("Converting " <> display filePath <> " to an absolute path")
  absPath <- absolutize filePath
  logDebug ("Converted file to absolute: " <> displayShow absPath)
  logDebug "Running main `purty` program"
  stream <- Purty.fromAbsFile absPath
  logDebug "Successfully created stream for rendering"
  logDebug (displayShow $ void stream)
  case output of
    InPlace -> liftIO $ withSystemTempFile "purty.purs" $ \fp h -> do
      renderIO h stream
      hClose h
      copyPermissions absPath fp
      copyFile fp absPath
    StdOut -> do
      logDebug "Printing to stdout"
      liftIO $ renderIO stdout stream
