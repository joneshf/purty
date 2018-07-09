module Env where

import "rio" RIO

import "prettyprinter" Data.Text.Prettyprint.Doc
    ( LayoutOptions(LayoutOptions, layoutPageWidth)
    , PageWidth(AvailablePerLine)
    )
import "dhall" Dhall                             (Inject, Interpret)
import "path" Path                               (Abs, File, Path, Rel)

import qualified "this" Log

data Config
  = Config
    { formatting :: !Formatting
    , output     :: !Output
    , verbosity  :: !Verbosity
    }
  deriving (Generic, Show)

instance Log.Inspect Config

instance Inject Config

instance Interpret Config

defaultConfig :: Config
defaultConfig =
  Config
    { formatting = Static
    , output = StdOut
    , verbosity = NotVerbose
    }

-- |
-- How we want to pretty print
--
-- Dynamic formatting takes line length into account.
-- Static formatting is always the same.
data Formatting
  = Dynamic
  | Static
  deriving (Generic, Show)

instance Log.Inspect Formatting

instance Inject Formatting

instance Interpret Formatting

-- |
-- What to do with the pretty printed output
data Output
  = InPlace
  | StdOut
  deriving (Generic, Show)

instance Log.Inspect Output

instance Inject Output

instance Interpret Output

defaultLayoutOptions :: LayoutOptions
defaultLayoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine 80 1 }

data PurtyFilePath
  = AbsFile !(Path Abs File)
  | RelFile !(Path Rel File)
  | Unparsed !Text
  deriving (Show)

instance Log.Inspect PurtyFilePath

-- |
-- The minimum level of logs to display.
--
-- 'Verbose' will display debug logs.
-- Debug logs are pretty noisy, but useful when diagnosing problems.
data Verbosity
  = Verbose
  | NotVerbose
  deriving (Eq, Generic, Show)

instance Log.Inspect Verbosity

instance Inject Verbosity

instance Interpret Verbosity
