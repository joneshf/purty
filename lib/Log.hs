{-# LANGUAGE ImplicitParams #-}
module Log where

import "rio" RIO

import "freer-simple" Control.Monad.Freer (Eff, Member, send)
import "base" GHC.Stack                   (callStack)

data Log a where
  Debug :: !CallStack -> !Utf8Builder -> Log ()
  Error :: !CallStack -> !Utf8Builder -> Log ()

debug :: (HasCallStack, Member Log e) => Utf8Builder -> Eff e ()
debug = send . Debug callStack

error :: (HasCallStack, Member Log e) => Utf8Builder -> Eff e ()
error = send . Error callStack

-- | We use a pretty bad hack here to get around the fact that `rio`
--   doesn't allow passing in the `CallStack`.
--   This is probably going to break on us one day.
io :: LogFunc -> Log a -> IO a
io logFunc = \case
  Debug x y -> let ?callStack = x in runReaderT (logDebug y) logFunc
  Error x y -> let ?callStack = x in runReaderT (logError y) logFunc
