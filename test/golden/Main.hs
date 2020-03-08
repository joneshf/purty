{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import qualified "bazel-runfiles" Bazel.Runfiles
import qualified "componentm" Control.Monad.Component
import qualified "bytestring" Data.ByteString.Builder
import qualified "purty" Error
import qualified "purty" Log
import qualified "purty" Purty
import "rio" RIO hiding (log)
import qualified "rio" RIO.Directory
import "rio" RIO.FilePath ((</>))
import qualified "rio" RIO.FilePath
import qualified "rio" RIO.Text
import qualified "pathwalk" System.Directory.PathWalk
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
      runfiles <- Bazel.Runfiles.create
      prefix <- Bazel.Runfiles.rlocation runfiles "com_gitlab_joneshf_purty/test/golden/files"
      hPutBuilder stdout ("prefix: " <> fromString prefix <> "\n")
      formattedAdo <- Bazel.Runfiles.rlocation runfiles "com_gitlab_joneshf_purty/test/golden/files/formatted/Ado.purs"
      hPutBuilder stdout ("formattedAdo: " <> fromString formattedAdo <> "\n")
      tests <- goldenTests log prefix
      Test.Tasty.defaultMain tests
  case result of
    Left ExitSuccess -> exitSuccess
    Left code -> exitWith code
    Right _ -> exitSuccess

diff :: FilePath -> FilePath -> [String]
diff old new = ["diff", "--unified", old, new]

files :: FilePath
files = "test" </> "golden" </> "files"

fileUsed :: FilePath -> FilePath -> Test.Tasty.TestTree
fileUsed prefix fullPath =
  Test.Tasty.HUnit.testCase
    (files </> "formatted" </> filename)
    exists
  where
    exists :: Test.Tasty.HUnit.Assertion
    exists = do
      originalExists <- RIO.Directory.doesFileExist (prefix </> "original" </> filename)
      unless
        originalExists
        ( Test.Tasty.HUnit.assertFailure
            ( (files </> "formatted" </> filename)
                <> " is unused."
                <> " Please add an original version at `"
                <> (files </> "original" </> filename)
                <> "."
            )
        )
    filename :: FilePath
    filename = RIO.FilePath.takeFileName fullPath

filesUsedTests :: FilePath -> IO Test.Tasty.TestTree
filesUsedTests prefix = do
  formatteds <- psFiles prefix "formatted"
  pure $ Test.Tasty.testGroup "filesUsed" (fmap (fileUsed prefix) formatteds)

formattingTests :: Log.Handle -> FilePath -> IO Test.Tasty.TestTree
formattingTests log prefix = do
  originals <- psFiles prefix "original"
  pure $ Test.Tasty.testGroup "formatting" (fmap (golden log prefix) originals)

golden :: Log.Handle -> FilePath -> FilePath -> Test.Tasty.TestTree
golden log prefix original =
  Test.Tasty.Golden.goldenVsStringDiff
    (files </> "formatted" </> RIO.FilePath.takeFileName original)
    diff
    (prefix </> "formatted" </> RIO.FilePath.takeFileName original)
    (test log original)

goldenTests :: Log.Handle -> FilePath -> IO Test.Tasty.TestTree
goldenTests log prefix = do
  filesUsed <- filesUsedTests prefix
  formatting <- formattingTests log prefix
  pure $
    Test.Tasty.testGroup
      "golden"
      [ filesUsed,
        formatting
      ]

psFiles :: FilePath -> FilePath -> IO [FilePath]
psFiles prefix dir = Test.Tasty.Golden.findByExtension [".purs"] (prefix </> dir)

test :: Log.Handle -> FilePath -> IO LByteString
test log file = do
  result <- withLazyFile file (Purty.format log)
  case result of
    Left err ->
      Test.Tasty.HUnit.assertFailure
        (RIO.Text.unpack $ utf8BuilderToText $ Error.format err)
    Right formatted ->
      pure (Data.ByteString.Builder.toLazyByteString $ getUtf8Builder formatted)
