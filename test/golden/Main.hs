{-# LANGUAGE QuasiQuotes #-}
module Main where

import "protolude" Protolude

import "path" Path (Path, Rel, File, (</>), relfile)
import "tasty" Test.Tasty (TestTree, testGroup, defaultMain)
import "tasty-golden" Test.Tasty.Golden (goldenVsString)
import "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text (renderLazy)
import "path-io" Path.IO (getCurrentDir)

import "purty" Purty (purty, runPurty, defaultEnv)

main :: IO ()
main = defaultMain goldenTests

goldenTests :: TestTree
goldenTests =
  testGroup
    "golden"
    [ goldenVsString "newtype record" "test/golden/files/NewtypeRecord.purs" (testPurty [relfile|test/golden/files/NewtypeRecord.purs|])
    , goldenVsString "data with parameters" "test/golden/files/DataWithParameters.purs" (testPurty [relfile|test/golden/files/DataWithParameters.purs|])
    , goldenVsString "sproxy" "test/golden/files/SProxy.purs" (testPurty [relfile|test/golden/files/SProxy.purs|])
    , goldenVsString "typeclass" "test/golden/files/TypeClass.purs" (testPurty [relfile|test/golden/files/TypeClass.purs|])
    , goldenVsString "type synonym" "test/golden/files/TypeSynonym.purs" (testPurty [relfile|test/golden/files/TypeSynonym.purs|])
    ]

testPurty :: Path Rel File -> IO LByteString
testPurty filePath = do
  cwd <- getCurrentDir
  result <- runPurty (defaultEnv $ cwd </> filePath) purty
  stream <- hush result
  pure (toS $ renderLazy stream)
