module Env where

import "rio" RIO

import "prettyprinter" Data.Text.Prettyprint.Doc
    ( LayoutOptions(LayoutOptions, layoutPageWidth)
    , PageWidth(AvailablePerLine, Unbounded)
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

instance HasFormatting Config where
  formattingL = lens formatting (\config formatting -> config { formatting })

instance HasOutput Config where
  outputL = lens output (\config output -> config { output })

instance HasVerbosity Config where
  verbosityL = lens verbosity (\config verbosity -> config { verbosity })

instance Inject Config

instance Interpret Config

class (HasFormatting env, HasOutput env, HasVerbosity env) => HasConfig env where
  configL :: Lens' env Config

defaultConfig :: Config
defaultConfig =
  Config
    { formatting = Static
    , output = StdOut
    , verbosity = NotVerbose
    }

data Env
  = Env
    { config            :: !Config
    , logFunc           :: !LogFunc
    , prettyPrintConfig :: !PrettyPrintConfig
    }

instance Display Env where
  display Env { config, prettyPrintConfig } =
    "{Config: "
      <> display config
      <> ", PrettyPrintConfig: "
      <> display prettyPrintConfig
      <> "}"

defaultEnv :: Formatting -> LogFunc -> Env
defaultEnv formatting logFunc =
  Env { config, logFunc, prettyPrintConfig }
    where
    config = Config { formatting, output, verbosity }
    prettyPrintConfig = defaultPrettyPrintConfig
    output = StdOut
    verbosity = Verbose

class (HasConfig env, HasLogFunc env, HasPrettyPrintConfig env) => HasEnv env where
  envL :: Lens' env Env

instance HasConfig Env where
  configL = lens config (\env config -> env { config })

instance HasEnv Env where
  envL = id

instance HasFormatting Env where
  formattingL = configL.formattingL

instance HasLayoutOptions Env where
  layoutOptionsL = prettyPrintConfigL.layoutOptionsL

instance HasLogFunc Env where
  logFuncL = lens logFunc (\env logFunc -> env { logFunc })

instance HasOutput Env where
  outputL = configL.outputL

instance HasPrettyPrintConfig Env where
  prettyPrintConfigL = lens prettyPrintConfig (\env prettyPrintConfig -> env { prettyPrintConfig })

instance HasVerbosity Env where
  verbosityL = configL.verbosityL
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

class HasFormatting env where
  formattingL :: Lens' env Formatting

instance HasFormatting Formatting where
  formattingL = id

class HasLayoutOptions env where
  layoutOptionsL :: Lens' env LayoutOptions

instance HasLayoutOptions LayoutOptions where
  layoutOptionsL = id

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

class HasOutput env where
  outputL :: Lens' env Output

instance HasOutput Output where
  outputL = id

newtype PrettyPrintConfig
  = PrettyPrintConfig
    { layoutOptions :: LayoutOptions
    }

instance Display PrettyPrintConfig where
  display PrettyPrintConfig { layoutOptions } =
    case layoutPageWidth layoutOptions of
      AvailablePerLine width ribbon ->
        "{Page width: "
          <> display width
          <> ", Ribbon width: "
          <> display (truncate (ribbon * fromIntegral width) :: Int)
          <> "}"
      Unbounded -> "Unbounded"

instance HasLayoutOptions PrettyPrintConfig where
  layoutOptionsL = lens layoutOptions (\config layoutOptions -> config { layoutOptions })

class (HasLayoutOptions env) => HasPrettyPrintConfig env where
  prettyPrintConfigL :: Lens' env PrettyPrintConfig

instance HasPrettyPrintConfig PrettyPrintConfig where
  prettyPrintConfigL = id

defaultPrettyPrintConfig :: PrettyPrintConfig
defaultPrettyPrintConfig =
  PrettyPrintConfig
    { layoutOptions =
      LayoutOptions
      { layoutPageWidth =
        AvailablePerLine 80 1
      }
    }

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

class HasVerbosity env where
  verbosityL :: Lens' env Verbosity

instance HasVerbosity Verbosity where
  verbosityL = id

