{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified "componentm" Control.Monad.Component
import qualified "bytestring" Data.ByteString.Builder
import qualified "purty" Error
import qualified "purty" Log
import qualified "purty" Purty
import "rio" RIO hiding (log)
import "rio" RIO.FilePath ((</>))
import qualified "rio" RIO.FilePath
import qualified "rio" RIO.Text
import qualified "tasty" Test.Tasty
import qualified "tasty-golden" Test.Tasty.Golden
import qualified "tasty-hunit" Test.Tasty.HUnit

main :: IO ()
main = do
  let config =
        Log.Config
          { Log.name = "Log",
            Log.verbose = False
          }
  result <- Control.Monad.Component.runComponentM "golden" (Log.handle config) $
    \log -> try $ do
      tests <- goldenTests log
      Test.Tasty.defaultMain tests
  case result of
    Left ExitSuccess -> exitSuccess
    Left code -> exitWith code
    Right _ -> exitSuccess

diff :: FilePath -> FilePath -> [String]
diff old new = ["diff", "--unified", old, new]

files :: FilePath
files = "test" </> "golden" </> "files"

fileUsed :: (Foldable f) => f FilePath -> FilePath -> Test.Tasty.TestTree
fileUsed haystack needle =
  Test.Tasty.HUnit.testCase
    needle
    ( Test.Tasty.HUnit.assertBool
        ( needle
            <> " is unused."
            <> "Please add an original version to the `original` directory."
        )
        (RIO.FilePath.takeBaseName needle `elem` haystack)
    )

filesUsedTests :: IO Test.Tasty.TestTree
filesUsedTests = do
  formatteds <- psFiles "formatted"
  originals' <- psFiles "original"
  let originals = fmap RIO.FilePath.takeBaseName originals'
  pure $ Test.Tasty.testGroup "filesUsed" (fmap (fileUsed originals) formatteds)

formattingTests :: Log.Handle -> IO Test.Tasty.TestTree
formattingTests log = do
  originals <- psFiles "original"
  pure $ Test.Tasty.testGroup "formatting" (fmap (golden log) originals)

golden :: Log.Handle -> FilePath -> Test.Tasty.TestTree
golden log original =
  Test.Tasty.Golden.goldenVsStringDiff
    goldenFile
    diff
    goldenFile
    (test log original)
  where
    goldenFile = files </> "formatted" </> RIO.FilePath.takeFileName original

goldenTests :: Log.Handle -> IO Test.Tasty.TestTree
goldenTests log = do
  filesUsed <- filesUsedTests
  formatting <- formattingTests log
  pure $
    Test.Tasty.testGroup
      "golden"
      [ filesUsed,
        formatting
      ]

psFiles :: FilePath -> IO [FilePath]
psFiles dir = Test.Tasty.Golden.findByExtension [".purs"] (files </> dir)

test :: Log.Handle -> FilePath -> IO LByteString
test log file = do
  result <- withLazyFile file (Purty.format log)
  case result of
    Left err ->
      Test.Tasty.HUnit.assertFailure
        (RIO.Text.unpack $ utf8BuilderToText $ Error.format err)
    Right formatted ->
      pure (Data.ByteString.Builder.toLazyByteString $ getUtf8Builder formatted)
