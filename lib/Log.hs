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
    { debug :: (HasCallStack) => Utf8Builder -> IO ()
    , info  :: (HasCallStack) => Utf8Builder -> IO ()
    }

handle :: Config -> Control.Monad.Component.ComponentM Handle
handle config = do
  (logFunc, _) <-
    Control.Monad.Component.buildComponent (name config) acquire release
  pure
    Handle
      { debug = GHC.Stack.withFrozenCallStack flip runReaderT logFunc . logDebug
      , info = GHC.Stack.withFrozenCallStack flip runReaderT logFunc . logInfo
      }
  where
  acquire :: IO (LogFunc, IO ())
  acquire = do
    options <- logOptionsHandle stderr (verbose config)
    newLogFunc options

  release :: (a, IO ()) -> IO ()
  release = snd
