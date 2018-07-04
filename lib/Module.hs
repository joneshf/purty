module Module (parse) where

import "rio" RIO

import "freer-simple" Control.Monad.Freer       (Eff, Members)
import "freer-simple" Control.Monad.Freer.Error (throwError, Error)
import "path" Path                              (Abs, File, Path, fromAbsFile)
import "parsec" Text.Parsec                     (ParseError)

import qualified "purescript" Language.PureScript

import qualified "this" File
import qualified "this" Log

parse ::
  (Members '[Error ParseError, File.File, Log.Log] e) =>
  Path Abs File ->
  Eff e Language.PureScript.Module
parse absFile = do
  contents <- File.read absFile
  Log.debug "Read file contents:"
  Log.debug (display contents)
  (_, m) <- parse' absFile contents
  pure m

parse' ::
  (Members '[Error ParseError] e) =>
  Path Abs File ->
  Text ->
  Eff e (FilePath, Language.PureScript.Module)
parse' absFile contents =
  either
    throwError
    pure
    (Language.PureScript.parseModuleFromFile id (fromAbsFile absFile, contents))
