{-# LANGUAGE ImplicitParams #-}
module Log where

import "rio" RIO

import "freer-simple" Control.Monad.Freer (Eff, Member, send)
import "base" GHC.Stack                   (callStack)

import qualified "prettyprinter" Data.Text.Prettyprint.Doc
import qualified "purescript" Language.PureScript
import qualified "path" Path

class (Show a) => Inspect a

instance Inspect ()
instance Inspect (Path.Path a b)
instance Inspect (Data.Text.Prettyprint.Doc.Doc a)
instance Inspect Data.Text.Prettyprint.Doc.LayoutOptions
instance (Inspect a) => Inspect (Data.Text.Prettyprint.Doc.SimpleDocStream a)
instance Inspect Language.PureScript.Module

data Log a where
  Debug :: !CallStack -> !Utf8Builder -> Log ()
  Inspect :: (Inspect b) => !CallStack -> b -> Log ()
  Error :: !CallStack -> !Utf8Builder -> Log ()

debug :: (HasCallStack, Member Log e) => Utf8Builder -> Eff e ()
debug = send . Debug callStack

inspect :: (HasCallStack, Inspect a, Member Log e) => a -> Eff e ()
inspect = send . Inspect callStack

error :: (HasCallStack, Member Log e) => Utf8Builder -> Eff e ()
error = send . Error callStack

-- | We use a pretty bad hack here to get around the fact that `rio`
--   doesn't allow passing in the `CallStack`.
--   This is probably going to break on us one day.
io :: LogFunc -> Log a -> IO a
io logFunc = \case
  Debug x y -> let ?callStack = x in runReaderT (logDebug y) logFunc
  Inspect x y ->
    let ?callStack = x in runReaderT (logDebug $ displayShow y) logFunc
  Error x y -> let ?callStack = x in runReaderT (logError y) logFunc
