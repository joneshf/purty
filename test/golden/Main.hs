{-# LANGUAGE QuasiQuotes #-}
module Main where

import "protolude" Protolude hiding (diff)

import "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text (renderLazy)
import "path" Path
    ( File
    , Path
    , Rel
    , relfile
    , (</>)
    )
import "path-io" Path.IO                                     (getCurrentDir)
import "tasty" Test.Tasty
    ( TestTree
    , defaultMain
    , testGroup
    )
import "tasty-golden" Test.Tasty.Golden
    ( goldenVsStringDiff
    )

import "purty" Purty (defaultEnv, purty, runPurty)

main :: IO ()
main = defaultMain goldenTests

diff :: FilePath -> FilePath -> [[Char]]
diff old new = ["diff", "--unified", old, new]

goldenTests :: TestTree
goldenTests =
  testGroup
    "golden"
    [ goldenVsStringDiff "newtype record" diff "test/golden/files/NewtypeRecord.purs" (testPurty [relfile|test/golden/files/NewtypeRecord.purs|])
    , goldenVsStringDiff "data with parameters" diff "test/golden/files/DataWithParameters.purs" (testPurty [relfile|test/golden/files/DataWithParameters.purs|])
    , goldenVsStringDiff "empty data" diff "test/golden/files/EmptyData.purs" (testPurty [relfile|test/golden/files/EmptyData.purs|])
    , goldenVsStringDiff "multi-parameter type class instance head" diff "test/golden/files/MPTCHead.purs" (testPurty [relfile|test/golden/files/MPTCHead.purs|])
    , goldenVsStringDiff "module header" diff "test/golden/files/ModuleHeader.purs" (testPurty [relfile|test/golden/files/ModuleHeader.purs|])
    , goldenVsStringDiff "sproxy" diff "test/golden/files/SProxy.purs" (testPurty [relfile|test/golden/files/SProxy.purs|])
    , goldenVsStringDiff "typeclass" diff "test/golden/files/TypeClass.purs" (testPurty [relfile|test/golden/files/TypeClass.purs|])
    , goldenVsStringDiff "type synonym" diff "test/golden/files/TypeSynonym.purs" (testPurty [relfile|test/golden/files/TypeSynonym.purs|])
    , goldenVsStringDiff "type synonym newline" diff "test/golden/files/TypeSynonymNewline.purs" (testPurty [relfile|test/golden/files/TypeSynonymNewline.purs|])
    ]

testPurty :: Path Rel File -> IO LByteString
testPurty filePath = do
  cwd <- getCurrentDir
  result <- runPurty (defaultEnv $ cwd </> filePath) purty
  stream <- hush result
  pure (toS $ renderLazy stream)
