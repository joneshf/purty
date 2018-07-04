module Exit where

import "rio" RIO

import "freer-simple" Control.Monad.Freer (Eff, Member, send)
import "base" System.Exit                 (exitFailure)

data Exit a where
  Failure :: Exit a

failure :: (Member Exit e) => Eff e a
failure = send Failure

io :: Exit a -> IO a
io = \case
  Failure -> exitFailure
