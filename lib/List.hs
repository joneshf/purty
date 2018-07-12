module List where

import "rio" RIO hiding (mapMaybe, toList)

import "base" GHC.Exts (IsList(Item, fromList, toList))
import "base" Data.List.NonEmpty (NonEmpty((:|)))
import "witherable" Data.Witherable (Filterable(mapMaybe))

import qualified "base" Data.List.NonEmpty

-- |
-- Differs from '[a]' in that the non-empty side has evidence of being nonempty.
--
-- This is beneficial in case analysis.
-- When you case on 'List a', you get the 'Data.List.NonEmpty.NonEmpty a'.
-- You don't have to re-case on things multiple times.
data List a
  = Empty
  | NonEmpty !(NonEmpty a)
  deriving (Functor, Show)

instance IsList (List a) where
  type Item (List a) = a

  fromList :: [a] -> List a
  fromList = \case
    [] -> Empty
    x : xs -> NonEmpty (x :| xs)

  toList :: List a -> [a]
  toList = \case
    Empty -> []
    NonEmpty (x :| xs) -> x : xs

instance Filterable List where
  mapMaybe :: (a -> Maybe b) -> List a -> List b
  mapMaybe f = fromList . mapMaybe f . toList

list' :: (Monoid b) => (NonEmpty a -> b) -> List a -> b
list' f = \case
  Empty -> mempty
  NonEmpty x -> f x

sortWith :: Ord b => (a -> b) -> List a -> List a
sortWith f = \case
  Empty -> Empty
  NonEmpty x -> NonEmpty (Data.List.NonEmpty.sortWith f x)
