module Comment where

import "rio" RIO

import "prettyprinter" Data.Text.Prettyprint.Doc (Doc, hardline, pretty)
import "base" GHC.Exts                           (IsList(fromList))

import qualified "purescript" Language.PureScript

import qualified "this" List

data Comment
  = Block !Text
  | Line !Text
  deriving (Show)

doc :: Comment -> Doc a
doc = \case
  Block x -> "{-" <> pretty x <> "-}"
  Line x -> "--" <> pretty x <> hardline

fromPureScript :: Language.PureScript.Comment -> Comment
fromPureScript = \case
  Language.PureScript.BlockComment x -> Block x
  Language.PureScript.LineComment x -> Line x

newtype Comments
  = Comments (List.List Comment)
  deriving (Show)

comments :: [Language.PureScript.Comment] -> Comments
comments = Comments . fromList . fmap fromPureScript

docFromComments :: Comments  -> Doc a
docFromComments = \case
  Comments x' -> List.list' (foldMap go) x'
    where
    go = \case
      x@Block {} -> doc x <> hardline
      x@Line {} -> doc x
