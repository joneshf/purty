module Main where

import "rio" RIO

import "prettyprinter" Data.Text.Prettyprint.Doc             (Doc, layoutSmart)
import "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import "purescript" Language.PureScript
    ( Type(ParensInType, RCons, REmpty, TypeApp, TypeVar)
    , tyRecord
    )
import "tasty" Test.Tasty
    ( TestTree
    , defaultMain
    , testGroup
    )
import "tasty-hunit" Test.Tasty.HUnit
    ( assertEqual
    , testCase
    )

import "purty" Env (defaultPrettyPrintConfig, layoutOptions)

import qualified "purty" Doc.Dynamic
import qualified "purty" Doc.Static

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests =
  testGroup
    "Doc"
    [ testGroup "record" recordTests
    , testGroup "row" rowTests
    ]

recordTests :: [TestTree]
recordTests =
  [ testGroup "Empty record" emptyRecordTests
  , testGroup "Open record" openRecordTests
  ]

emptyRecordTests :: [TestTree]
emptyRecordTests =
  [ testGroup
    "Empty record types have the same spacing in both formatters"
    [ testCase
      "dynamic"
      (assertEqual "" "{}" (renderText $ Doc.Dynamic.fromType emptyRecord))
    , testCase
      "static"
      (assertEqual "" "{}" (renderText $ Doc.Static.fromType emptyRecord))
    ]
  , testGroup
    "Empty record types in parens have the same spacing in both formatters"
    [ testCase
      "dynamic"
      (assertEqual "" "({})" (renderText $ Doc.Dynamic.fromType $ ParensInType emptyRecord))
    , testCase
      "static"
      (assertEqual "" "({})" (renderText $ Doc.Static.fromType $ ParensInType emptyRecord))
    ]
  ]

openRecordTests :: [TestTree]
openRecordTests =
  [ testGroup
    "Open record types have the same spacing in both formatters"
    [ testCase
      "dynamic"
      (assertEqual "" "{ | foo}" (renderText $ Doc.Dynamic.fromType openRecord))
    , testCase
      "static"
      (assertEqual "" "{ | foo}" (renderText $ Doc.Static.fromType openRecord))
    ]
  , testGroup
    "Open record types in parens have the same spacing in both formatters"
    [ testCase
      "dynamic"
      (assertEqual "" "({ | foo})" (renderText $ Doc.Dynamic.fromType $ ParensInType openRecord))
    , testCase
      "static"
      (assertEqual "" "({ | foo})" (renderText $ Doc.Static.fromType $ ParensInType openRecord))
    ]
  , testGroup
    "Open record types with one label have the same spacing in both formatters"
    [ testCase
      "dynamic"
      (assertEqual "" "{a :: b | r}" (renderText $ Doc.Dynamic.fromType $ TypeApp tyRecord openRow1))
    , testCase
      "static"
      (assertEqual "" "{a :: b | r}" (renderText $ Doc.Static.fromType $ TypeApp tyRecord openRow1))
    ]
  , testGroup
    "Open record types with one label in parens have the same spacing in both formatters"
    [ testCase
      "dynamic"
      (assertEqual "" "({a :: b | r})" (renderText $ Doc.Dynamic.fromType $ ParensInType $ TypeApp tyRecord openRow1))
    , testCase
      "static"
      (assertEqual "" "({a :: b | r})" (renderText $ Doc.Static.fromType $ ParensInType $ TypeApp tyRecord openRow1))
    ]
  , testGroup
    "Open record types with multiple labels have the same spacing in both formatters"
    [ testCase
      "dynamic"
      (assertEqual "" "{a :: b, c :: d | r}" (renderText $ Doc.Dynamic.fromType $ TypeApp tyRecord openRow2))
    , testCase
      "static"
      (assertEqual "" "{a :: b, c :: d | r}" (renderText $ Doc.Static.fromType $ TypeApp tyRecord openRow2))
    ]
  , testGroup
    "Open record types with multiple labels in parens have the same spacing in both formatters"
    [ testCase
      "dynamic"
      (assertEqual "" "({a :: b, c :: d | r})" (renderText $ Doc.Dynamic.fromType $ ParensInType $ TypeApp tyRecord openRow2))
    , testCase
      "static"
      (assertEqual "" "({a :: b, c :: d | r})" (renderText $ Doc.Static.fromType $ ParensInType $ TypeApp tyRecord openRow2))
    ]
  ]

rowTests :: [TestTree]
rowTests =
  [ testGroup
    "Open row types with one label have the same spacing in both formatters"
    [ testCase
      "dynamic"
      (assertEqual "" "(a :: b | r)" (renderText $ Doc.Dynamic.fromType openRow1))
    , testCase
      "static"
      (assertEqual "" "(a :: b | r)" (renderText $ Doc.Static.fromType openRow1))
    ]
  , testGroup
    "Open row types with one label in parens have the same spacing in both formatters"
    [ testCase
      "dynamic"
      (assertEqual "" "((a :: b | r))" (renderText $ Doc.Dynamic.fromType $ ParensInType openRow1))
    , testCase
      "static"
      (assertEqual "" "((a :: b | r))" (renderText $ Doc.Static.fromType $ ParensInType openRow1))
    ]
  , testGroup
    "Open row types with multiple labels have the same spacing in both formatters"
    [ testCase
      "dynamic"
      (assertEqual "" "(a :: b, c :: d | r)" (renderText $ Doc.Dynamic.fromType openRow2))
    , testCase
      "static"
      (assertEqual "" "(a :: b, c :: d | r)" (renderText $ Doc.Static.fromType openRow2))
    ]
  , testGroup
    "Open row types with multiple labels in parens have the same spacing in both formatters"
    [ testCase
      "dynamic"
      (assertEqual "" "((a :: b, c :: d | r))" (renderText $ Doc.Dynamic.fromType $ ParensInType openRow2))
    , testCase
      "static"
      (assertEqual "" "((a :: b, c :: d | r))" (renderText $ Doc.Static.fromType $ ParensInType openRow2))
    ]
  ]

emptyRecord :: Language.PureScript.Type
emptyRecord = TypeApp tyRecord REmpty

openRecord :: Language.PureScript.Type
openRecord = TypeApp tyRecord (TypeVar "foo")

openRow1 :: Language.PureScript.Type
openRow1 = RCons "a" (TypeVar "b") (TypeVar "r")

openRow2 :: Language.PureScript.Type
openRow2 = RCons "a" (TypeVar "b") (RCons "c" (TypeVar "d") (TypeVar "r"))

renderText :: Doc a -> Text
renderText = renderStrict . layoutSmart (layoutOptions defaultPrettyPrintConfig)
