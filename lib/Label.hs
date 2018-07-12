module Label where

import "rio" RIO

import "prettyprinter" Data.Text.Prettyprint.Doc (Doc, pretty)

import qualified "purescript" Language.PureScript
import qualified "purescript" Language.PureScript.Label
import qualified "purescript" Language.PureScript.PSString

-- |
-- We're using the underlying PureScript representation here,
-- as it handles unicode properly for the language.
newtype Label
  = Label Language.PureScript.Label.Label
  deriving (Eq, IsString, Show)

doc :: Label -> Doc a
doc = \case
  Label x -> pretty (Language.PureScript.prettyPrintLabel x)

fromPureScript :: Language.PureScript.Label.Label -> Label
fromPureScript = Label

fromPSString :: Language.PureScript.PSString.PSString -> Label
fromPSString = fromPureScript . Language.PureScript.Label.Label

fromText :: Text -> Label
fromText = fromPSString . Language.PureScript.PSString.mkString
