module Log
  ( Config(..)
  , Handle
  , debug
  , handle
  ) where

import "rio" RIO hiding (Handle, handle)

import qualified "base" GHC.Stack
import qualified "componentm" Control.Monad.Component

data Config
  = Config
    { name :: Text
    , verbose :: Bool
    }

data Handle
  = Handle
    { debug :: (HasCallStack) => Utf8Builder -> IO ()
    }

handle :: Config -> Control.Monad.Component.ComponentM Handle
handle config = do
  (logFunc, _) <-
    Control.Monad.Component.buildComponent (name config) acquire release
  pure
    Handle
      { debug = GHC.Stack.withFrozenCallStack debug' logFunc }
  where
  acquire :: IO (LogFunc, IO ())
  acquire = do
    options <- logOptionsHandle stderr (verbose config)
    newLogFunc options

  debug' :: (HasCallStack) => LogFunc -> Utf8Builder -> IO ()
  debug' logFunc message =
    GHC.Stack.withFrozenCallStack runReaderT (logDebug message) logFunc

  release :: (a, IO ()) -> IO ()
  release = snd
