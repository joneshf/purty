{-# LANGUAGE QuasiQuotes #-}
module Main where

import "rio" RIO

import "base" Control.Applicative                            (empty)
import "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
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

import "purty" Purty (defaultEnv, purty)

main :: IO ()
main = defaultMain goldenTests

diff :: FilePath -> FilePath -> [String]
diff old new = ["diff", "--unified", old, new]

golden :: TestName -> Path Rel File -> TestTree
golden testName goldenFile =
  goldenVsStringDiff testName diff (toFilePath goldenFile) $ do
    absFile <- makeAbsolute goldenFile
    (_, logOptions) <- logOptionsMemory
    withLogFunc logOptions $ \logFunc -> do
      result <- runRIO (defaultEnv logFunc absFile) $ purty absFile
      stream <- either (const empty) pure result
      pure (fromStrictBytes $ encodeUtf8 $ renderStrict stream)

goldenTests :: TestTree
goldenTests =
  testGroup
    "golden"
    [ golden "data with parameters" [relfile|test/golden/files/DataWithParameters.purs|]
    , golden "empty data" [relfile|test/golden/files/EmptyData.purs|]
    , golden "typeclass instance" [relfile|test/golden/files/Instance.purs|]
    , golden "module header" [relfile|test/golden/files/ModuleHeader.purs|]
    , golden "multi-parameter type class instance head" [relfile|test/golden/files/MPTCHead.purs|]
    , golden "newtype record" [relfile|test/golden/files/NewtypeRecord.purs|]
    , golden "record puns" [relfile|test/golden/files/RecordPuns.purs|]
    , golden "sproxy" [relfile|test/golden/files/SProxy.purs|]
    , golden "type synonym newline" [relfile|test/golden/files/TypeSynonymNewline.purs|]
    , golden "type synonym" [relfile|test/golden/files/TypeSynonym.purs|]
    , golden "typeclass" [relfile|test/golden/files/TypeClass.purs|]
    , golden "open record types" [relfile|test/golden/files/Merge.purs|]
    ]
