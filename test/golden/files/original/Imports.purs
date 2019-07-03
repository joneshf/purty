module Foo where

import Prelewd
import Prelude

import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Functor (class Functor, map, void)
import Data.List (cons, nil)
import Data.Maybe (Maybe (Just, Nothing), maybe)
import Prelude
  ( class EuclideanRing
  , class Semiring
  , Ordering (EQ, GT, LT)
  , Unit
  , Void
  , compose
  , flap
  , one
  , (&&)
  , (<<<)
  , (~>)
  )

import Data.Array (head
 , tail) as Array
import Data.List as List
import Data.Maybe
             as Maybe
