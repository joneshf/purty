module Main where

import qualified "purty" Args
import qualified "base" System.Exit
import qualified "base" System.IO
import qualified "purty" Purty

main :: System.IO.IO ()
main = do
  args <- Args.parse
  code <- Purty.run args
  System.Exit.exitWith code
