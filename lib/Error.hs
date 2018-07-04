module Error where

import "rio" RIO

import "freer-simple" Control.Monad.Freer (Eff, Members)
import "parsec" Text.Parsec               (ParseError)

import qualified "this" AST
import qualified "this" Exit
import qualified "this" Log

ast :: (Members '[Exit.Exit, Log.Log] e) => AST.Error -> Eff e a
ast = \case
  AST.MissingName -> do
    Log.error "Problem converting to our AST"
    Log.error (display AST.MissingName)
    Exit.failure

parseError :: (Members '[Exit.Exit, Log.Log] e) => ParseError -> Eff e a
parseError err = do
  Log.error "Problem parsing module"
  Log.error (displayShow err)
  Exit.failure
