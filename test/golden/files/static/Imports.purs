module Foo where

import Prelewd
import Prelude

import Control.Monad.Eff.Console
  ( CONSOLE
  , log
  )
import Data.Functor
  ( class Functor
  , map
  , void
  )
import Data.List
  ( cons
  , nil
  )
import Data.Maybe
  ( Maybe(Just, Nothing)
  , maybe
  )
import Prelude
  ( class Semiring
  , Ordering(LT, EQ, GT)
  , Void
  , Unit
  , compose
  , class EuclideanRing
  , one
  , flap
  , (<<<)
  , (~>)
  , (&&)
  )

import Data.List as List