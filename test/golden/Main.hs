{-# LANGUAGE QuasiQuotes #-}
module Main where

import "rio" RIO

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

import "purty" Env   (Formatting(Dynamic, Static), defaultEnv)
import "purty" Error (errors)
import "purty" Purty (purty)

import qualified "purty" App

main :: IO ()
main = defaultMain goldenTests

diff :: FilePath -> FilePath -> [String]
diff old new = ["diff", "--unified", old, new]

golden :: Formatting -> TestName -> Path Rel File -> TestTree
golden formatting testName goldenFile =
  goldenVsStringDiff testName diff (toFilePath goldenFile) $ do
    absFile <- makeAbsolute goldenFile
    (_, logOptions) <- logOptionsMemory
    withLogFunc logOptions $ \logFunc -> do
      let env = defaultEnv formatting logFunc
      stream <- App.run env (purty absFile `App.handle` errors)
      pure (fromStrictBytes $ encodeUtf8 $ renderStrict stream)

goldenTests :: TestTree
goldenTests =
  testGroup
    "golden"
    [ dynamic
    , static
    ]

dynamic :: TestTree
dynamic =
  testGroup
    "dynamic"
    [ golden Dynamic "ado" [relfile|test/golden/files/dynamic/Ado.purs|]
    , golden Dynamic "booleans" [relfile|test/golden/files/dynamic/Booleans.purs|]
    , golden Dynamic "char" [relfile|test/golden/files/dynamic/Char.purs|]
    , golden Dynamic "data with parameters" [relfile|test/golden/files/dynamic/DataWithParameters.purs|]
    , golden Dynamic "empty data" [relfile|test/golden/files/dynamic/EmptyData.purs|]
    , golden Dynamic "imports" [relfile|test/golden/files/dynamic/Imports.purs|]
    , golden Dynamic "typeclass instance" [relfile|test/golden/files/dynamic/Instance.purs|]
    , golden Dynamic "instance chain" [relfile|test/golden/files/dynamic/InstanceChain.purs|]
    , golden Dynamic "let" [relfile|test/golden/files/dynamic/Let.purs|]
    , golden Dynamic "long type signature" [relfile|test/golden/files/dynamic/LongTypeSignature.purs|]
    , golden Dynamic "multi-parameter type class instance head" [relfile|test/golden/files/dynamic/MPTCHead.purs|]
    , golden Dynamic "open record types" [relfile|test/golden/files/dynamic/Merge.purs|]
    , golden Dynamic "module header" [relfile|test/golden/files/dynamic/ModuleHeader.purs|]
    , golden Dynamic "operators" [relfile|test/golden/files/dynamic/Operators.purs|]
    , golden Dynamic "newtype record" [relfile|test/golden/files/dynamic/NewtypeRecord.purs|]
    , golden Dynamic "record puns" [relfile|test/golden/files/dynamic/RecordPuns.purs|]
    , golden Dynamic "sproxy" [relfile|test/golden/files/dynamic/SProxy.purs|]
    , golden Dynamic "typeclass" [relfile|test/golden/files/dynamic/TypeClass.purs|]
    , golden Dynamic "type synonym" [relfile|test/golden/files/dynamic/TypeSynonym.purs|]
    , golden Dynamic "type synonym newline" [relfile|test/golden/files/dynamic/TypeSynonymNewline.purs|]
    , golden Dynamic "where" [relfile|test/golden/files/dynamic/Where.purs|]
    ]

static :: TestTree
static =
  testGroup
    "static"
    [ golden Static "ado" [relfile|test/golden/files/static/Ado.purs|]
    , golden Static "booleans" [relfile|test/golden/files/static/Booleans.purs|]
    , golden Static "char" [relfile|test/golden/files/static/Char.purs|]
    , golden Static "data with parameters" [relfile|test/golden/files/static/DataWithParameters.purs|]
    , golden Static "empty data" [relfile|test/golden/files/static/EmptyData.purs|]
    , golden Static "imports" [relfile|test/golden/files/static/Imports.purs|]
    , golden Static "typeclass instance" [relfile|test/golden/files/static/Instance.purs|]
    , golden Static "instance chain" [relfile|test/golden/files/static/InstanceChain.purs|]
    , golden Static "let" [relfile|test/golden/files/static/Let.purs|]
    , golden Static "long type signature" [relfile|test/golden/files/static/LongTypeSignature.purs|]
    , golden Static "multi-parameter type class instance head" [relfile|test/golden/files/static/MPTCHead.purs|]
    , golden Static "open record types" [relfile|test/golden/files/static/Merge.purs|]
    , golden Static "module header" [relfile|test/golden/files/static/ModuleHeader.purs|]
    , golden Static "newtype record" [relfile|test/golden/files/static/NewtypeRecord.purs|]
    , golden Static "operators" [relfile|test/golden/files/static/Operators.purs|]
    , golden Static "record puns" [relfile|test/golden/files/static/RecordPuns.purs|]
    , golden Static "sproxy" [relfile|test/golden/files/static/SProxy.purs|]
    , golden Static "typeclass" [relfile|test/golden/files/static/TypeClass.purs|]
    , golden Static "type synonym" [relfile|test/golden/files/static/TypeSynonym.purs|]
    , golden Static "type synonym newline" [relfile|test/golden/files/static/TypeSynonymNewline.purs|]
    , golden Static "where" [relfile|test/golden/files/static/Where.purs|]
    ]
