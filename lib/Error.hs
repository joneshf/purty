module Error where

import "rio" RIO

import "freer-simple" Control.Monad.Freer       (Eff, Member)
import "freer-simple" Control.Monad.Freer.Error (handleError)
import "freer-simple" Data.OpenUnion            ((:++:))
import "parsec" Text.Parsec                     (ParseError)

import qualified "this" Declaration.Class
import qualified "this" Declaration.DataType
import qualified "this" Declaration.Fixity
import qualified "this" Declaration.Instance
import qualified "this" Declaration.Value
import qualified "this" Exit
import qualified "this" Export
import qualified "this" Kind
import qualified "this" Log
import qualified "this" Name
import qualified "this" Type

declarationClass ::
  (Member Exit.Exit e, Member Log.Log e) =>
  Eff (Declaration.Class.Errors :++: e) a ->
  Eff e a
declarationClass x =
  x `handleError` go
    `handleError` go
  where
  go err = do
    Log.error "Problem converting a type class"
    Log.error (display err)
    Exit.failure

declarationDataType ::
  (Member Exit.Exit e, Member Log.Log e) =>
  Eff (Declaration.DataType.Errors :++: e) a ->
  Eff e a
declarationDataType x =
  x `handleError` go
  where
  go err = do
    Log.error "Problem converting the declarations"
    Log.error (display err)
    Exit.failure

declarationFixity ::
  (Member Exit.Exit e, Member Log.Log e) =>
  Eff (Declaration.Fixity.Errors :++: e) a ->
  Eff e a
declarationFixity x =
  x `handleError` go
  where
  go err = do
    Log.error "Problem converting a fixity"
    Log.error (display err)
    Exit.failure

declarationInstance ::
  (Member Exit.Exit e, Member Log.Log e) =>
  Eff (Declaration.Instance.Errors :++: e) a ->
  Eff e a
declarationInstance x =
  x `handleError` go
    `handleError` go
    `handleError` go
    `handleError` go
    `handleError` go
  where
  go err = do
    Log.error "Problem converting a instance"
    Log.error (display err)
    Exit.failure

declarationValue ::
  (Member Exit.Exit e, Member Log.Log e) =>
  Eff (Declaration.Value.Errors :++: e) a ->
  Eff e a
declarationValue x =
  x `handleError` go
    `handleError` go
    `handleError` go
    `handleError` go
    `handleError` go
    `handleError` go
    `handleError` go
    `handleError` go
    `handleError` go
    `handleError` go
    `handleError` go
    `handleError` go
    `handleError` go
    `handleError` go
    `handleError` go
    `handleError` go
  where
  go err = do
    Log.error "Problem converting a value"
    Log.error (display err)
    Exit.failure

export ::
  (Member Exit.Exit e, Member Log.Log e) =>
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

kind ::
  (Member Exit.Exit e, Member Log.Log e) =>
  Eff (Kind.Errors :++: e) a ->
  Eff e a
kind x =
  x `handleError` go
  where
  go err = do
    Log.error "Problem converting a kind"
    Log.error (display err)
    Exit.failure

name ::
  (Member Exit.Exit e, Member Log.Log e) =>
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

parseError :: (Member Exit.Exit e, Member Log.Log e) => ParseError -> Eff e a
parseError err = do
  Log.error "Problem parsing module"
  Log.error (displayShow err)
  Exit.failure

type' ::
  (Member Exit.Exit e, Member Log.Log e) =>
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
