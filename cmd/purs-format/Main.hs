{-# LANGUAGE PackageImports #-}

module Main where

import qualified "this" Args
import qualified "base" System.Exit
import qualified "base" System.IO

main :: System.IO.IO ()
main = do
  args <- Args.parse
  code <- Args.run args
  System.Exit.exitWith code
