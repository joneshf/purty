{-# LANGUAGE PackageImports #-}

module Main where

import qualified "this" Args
import qualified "base" System.Exit
import qualified "base" System.IO

main :: System.IO.IO ()
main = do
  code <- Args.run
  System.Exit.exitWith code
