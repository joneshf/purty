module Env where

import "rio" RIO

import "prettyprinter" Data.Text.Prettyprint.Doc
    ( LayoutOptions(LayoutOptions, layoutPageWidth)
    , PageWidth(AvailablePerLine)
    )
import "dhall" Dhall                             (Inject, Interpret)
import "path" Path
    ( Abs
    , File
    , Path
    , Rel
    , fromAbsFile
    , fromRelFile
    )
import "path-io" Path.IO                         (makeAbsolute, resolveFile')
import "rio" RIO.Text                            (unpack)

data Config
  = Config
    { formatting :: !Formatting
    , output     :: !Output
    , verbosity  :: !Verbosity
    }
  deriving (Generic)

instance Display Config where
  display Config { formatting, verbosity, output } =
    "{"
      <> display formatting
      <> ", "
      <> display output
      <> ", "
      <> display verbosity
      <> "}"

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
  deriving (Generic)

instance Display Formatting where
  display = \case
    Dynamic -> "Dynamic"
    Static -> "Static"

instance Inject Formatting

instance Interpret Formatting

-- |
-- What to do with the pretty printed output
data Output
  = InPlace
  | StdOut
  deriving (Generic)

instance Display Output where
  display = \case
    InPlace -> "Formatting files in-place"
    StdOut -> "Writing formatted files to stdout"

instance Inject Output

instance Interpret Output

defaultLayoutOptions :: LayoutOptions
defaultLayoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine 80 1 }

data PurtyFilePath
  = AbsFile !(Path Abs File)
  | RelFile !(Path Rel File)
  | Unparsed !Text

instance Display PurtyFilePath where
  display = \case
    AbsFile path -> "Absolute file: " <> displayShow (fromAbsFile path)
    RelFile path -> "Relative file: " <> displayShow (fromRelFile path)
    Unparsed path -> "Unparsed: " <> displayShow path

absolutize :: MonadIO m => PurtyFilePath -> m (Path Abs File)
absolutize fp = case fp of
  AbsFile absolute -> pure absolute
  RelFile relative -> makeAbsolute relative
  Unparsed path    -> resolveFile' (unpack path)

-- |
-- The minimum level of logs to display.
--
-- 'Verbose' will display debug logs.
-- Debug logs are pretty noisy, but useful when diagnosing problems.
data Verbosity
  = Verbose
  | NotVerbose
  deriving (Eq, Generic)

instance Display Verbosity where
  display = \case
    Verbose -> "Verbose"
    NotVerbose -> "Not verbose"

instance Inject Verbosity

instance Interpret Verbosity
