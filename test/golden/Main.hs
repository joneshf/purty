{-# LANGUAGE QuasiQuotes #-}
module Main where

import "protolude" Protolude hiding (diff)

import "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text (renderLazy)
import "path" Path
    ( File
    , Path
    , Rel
    , relfile
    , toFilePath
    )
import "path-io" Path.IO                                     (makeAbsolute)
import "tasty" Test.Tasty
    ( TestName
    , TestTree
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

golden :: TestName -> Path Rel File -> TestTree
golden testName goldenFile =
  goldenVsStringDiff testName diff (toFilePath goldenFile) $ do
    absFile <- makeAbsolute goldenFile
    result <- runPurty (defaultEnv absFile) purty
    stream <- hush result
    pure (toS $ renderLazy stream)

goldenTests :: TestTree
goldenTests =
  testGroup
    "golden"
    [ golden "newtype record" [relfile|test/golden/files/NewtypeRecord.purs|]
    , golden "data with parameters" [relfile|test/golden/files/DataWithParameters.purs|]
    , golden "empty data" [relfile|test/golden/files/EmptyData.purs|]
    , golden "multi-parameter type class instance head" [relfile|test/golden/files/MPTCHead.purs|]
    , golden "module header" [relfile|test/golden/files/ModuleHeader.purs|]
    , golden "sproxy" [relfile|test/golden/files/SProxy.purs|]
    , golden "typeclass" [relfile|test/golden/files/TypeClass.purs|]
    , golden "type synonym" [relfile|test/golden/files/TypeSynonym.purs|]
    , golden "type synonym newline" [relfile|test/golden/files/TypeSynonymNewline.purs|]
    ]
