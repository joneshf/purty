{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams   #-}
{-# LANGUAGE PackageImports   #-}
module Log
  ( Config(..)
  , Handle
  , debug
  , handle
  , info
  ) where

import "rio" RIO hiding (Handle, handle)

import qualified "componentm" Control.Monad.Component
import qualified "base" GHC.Stack

data Config
  = Config
    { name    :: Text
    , verbose :: Bool
    }

data Handle
  = Handle
    { debug' :: CallStack -> Utf8Builder -> IO ()
    , info'  :: CallStack -> Utf8Builder -> IO ()
    }

debug :: HasCallStack => Handle -> Utf8Builder -> IO ()
debug handle' = debug' handle' GHC.Stack.callStack

-- | We use a pretty bad hack here to get around the fact that `rio` doesn't
-- allow passing in the `CallStack`. We take the explicit `CallStack`, and turn
-- it into an implicit param that `rio`s log functions pick up.
-- This is probably going to break on us one day.
handle :: Config -> Control.Monad.Component.ComponentM Handle
handle config = do
  (logFunc, _) <-
    Control.Monad.Component.buildComponent (name config) acquire release
  pure
    Handle
      { debug' = \callStack message -> do
        let ?callStack = callStack
        runReaderT (logDebug message) logFunc
      , info' = \callStack message -> do
        let ?callStack = callStack
        runReaderT (logInfo message) logFunc
      }
  where
  acquire :: IO (LogFunc, IO ())
  acquire = do
    options <- logOptionsHandle stderr (verbose config)
    newLogFunc options

  release :: (a, IO ()) -> IO ()
  release = snd

info :: HasCallStack => Handle -> Utf8Builder -> IO ()
info handle' = info' handle' GHC.Stack.callStack
