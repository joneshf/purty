module Variations where

import "rio" RIO

import "base" Data.List.NonEmpty                 (NonEmpty)
import "semigroupoids" Data.Semigroup.Foldable   (intercalateMap1)
import "prettyprinter" Data.Text.Prettyprint.Doc
    ( Doc
    , align
    , braces
    , brackets
    , comma
    , line
    , parens
    , space
    )

data Variations a
  = Variations
    { multiLine  :: !a
    , singleLine :: !a
    }
  deriving (Functor)

instance Applicative Variations where
  pure x = Variations x x
  Variations f g <*> Variations x y = Variations (f x) (g y)

instance (Semigroup a) => Semigroup (Variations a) where
  Variations w x <> Variations y z = Variations (w <> y) (x <> z)

bracesize :: (a -> Variations (Doc b)) -> NonEmpty a -> Variations (Doc b)
bracesize f xs =
  Variations
    { multiLine = align (braces (space <> intercalateMap1 (line <> comma <> space) (multiLine . f) xs <> line))
    , singleLine = braces (intercalateMap1 (comma <> space) (singleLine . f) xs)
    }

bracketesize :: (a -> Variations (Doc b)) -> NonEmpty a -> Variations (Doc b)
bracketesize f xs =
  Variations
    { multiLine = align (brackets (space <> intercalateMap1 (line <> comma <> space) (multiLine . f) xs <> line))
    , singleLine = brackets (intercalateMap1 (comma <> space) (singleLine . f) xs)
    }

parenthesize :: (a -> Variations (Doc b)) -> NonEmpty a -> Variations (Doc b)
parenthesize f xs =
  Variations
    { multiLine = align (parens (space <> intercalateMap1 (line <> comma <> space) (multiLine . f) xs <> line))
    , singleLine = parens (intercalateMap1 (comma <> space) (singleLine . f) xs)
    }
