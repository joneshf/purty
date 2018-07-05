module Error where

import "rio" RIO

import "freer-simple" Control.Monad.Freer       (Eff, Members)
import "freer-simple" Control.Monad.Freer.Error (handleError)
import "freer-simple" Data.OpenUnion            ((:++:))
import "parsec" Text.Parsec                     (ParseError)

import qualified "this" Declaration
import qualified "this" Exit
import qualified "this" Export
import qualified "this" Kind
import qualified "this" Log
import qualified "this" Name
import qualified "this" Type

declaration ::
  (Members '[Exit.Exit, Log.Log] e) =>
  Eff (Declaration.Errors :++: e) a ->
  Eff e a
declaration x =
  x `handleError` go
  where
  go err = do
    Log.error "Problem converting the declarations"
    Log.error (display err)
    Exit.failure

kind ::
  (Members '[Exit.Exit, Log.Log] e) =>
  Eff (Kind.Errors :++: e) a ->
  Eff e a
kind x =
  x `handleError` go
  where
  go err = do
    Log.error "Problem converting a kind"
    Log.error (display err)
    Exit.failure

export ::
  (Members '[Exit.Exit, Log.Log] e) =>
  Eff (Export.Errors :++: e) a ->
  Eff e a
export x =
  x `handleError` go
    `handleError` go
    `handleError` go
    `handleError` go
  where
  go err = do
    Log.error "Problem converting the exports"
    Log.error (display err)
    Exit.failure

name ::
  (Members '[Exit.Exit, Log.Log] e) =>
  Eff (Name.Errors :++: e) a ->
  Eff e a
name x =
  x `handleError` go
    `handleError` go
  where
  go err = do
    Log.error "Problem converting a name"
    Log.error (display err)
    Exit.failure

parseError :: (Members '[Exit.Exit, Log.Log] e) => ParseError -> Eff e a
parseError err = do
  Log.error "Problem parsing module"
  Log.error (displayShow err)
  Exit.failure

type' ::
  (Members '[Exit.Exit, Log.Log] e) =>
  Eff (Type.Errors :++: e) a ->
  Eff e a
type' x =
  x `handleError` go
    `handleError` go
    `handleError` go
    `handleError` go
    `handleError` go
    `handleError` go
    `handleError` go
    `handleError` go
  where
  go err = do
    Log.error "Problem converting a type"
    Log.error (display err)
    Exit.failure
