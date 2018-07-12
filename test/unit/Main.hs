module Main where

import "rio" RIO

import "base" Data.List.NonEmpty                             (NonEmpty((:|)))
import "prettyprinter" Data.Text.Prettyprint.Doc             (Doc, layoutSmart)
import "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import "tasty" Test.Tasty
    ( TestTree
    , defaultMain
    , testGroup
    )
import "tasty-hunit" Test.Tasty.HUnit
    ( assertEqual
    , testCase
    )

import "purty" Env (defaultLayoutOptions)

import qualified "purty" Type
import qualified "purty" Variations

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
  [ testCase
    "Empty record types have no spacing"
    (assertEqual "" "{}" (renderText $ Type.docFromRow emptyRecord))
  , testGroup
    "Empty record types in parens have the same spacing in both formatters"
    [ testCase
      "single line"
      (assertEqual "" "({})" (renderText $ Variations.singleLine $ Type.doc $ Type.TypeParens $ Type.TypeRow emptyRecord))
    , testCase
      "multi line"
      (assertEqual "" "({})" (renderText $ Variations.multiLine $ Type.doc $ Type.TypeParens $ Type.TypeRow emptyRecord))
    ]
  ]

openRecordTests :: [TestTree]
openRecordTests =
  [ testCase
    "Open record types have the same spacing in both formatters"
    (assertEqual "" "{ | foo}" (renderText $ Type.docFromRow openRecord1))
  , testGroup
    "Open record types in parens have the same spacing in both formatters"
    [ testCase
      "single line"
      (assertEqual "" "({ | foo})" (renderText $ Variations.singleLine $ Type.doc $ Type.TypeParens $ Type.TypeRow openRecord1))
    , testCase
      "multi line"
      (assertEqual "" "({ | foo})" (renderText $ Variations.multiLine $ Type.doc $ Type.TypeParens $ Type.TypeRow openRecord1))
    ]
  , testCase
    "Open record types with one label have the same spacing in both formatters"
    (assertEqual "" "{a :: b | r}" (renderText $ Type.docFromRow openRecord2))
  , testGroup
    "Open record types with one label in parens have the same spacing in both formatters"
    [ testCase
      "single line"
      (assertEqual "" "({a :: b | r})" (renderText $ Variations.singleLine $ Type.doc $ Type.TypeParens $ Type.TypeRow openRecord2))
    , testCase
      "multi line"
      (assertEqual "" "({a :: b | r})" (renderText $ Variations.multiLine $ Type.doc $ Type.TypeParens $ Type.TypeRow openRecord2))
    ]
  , testCase
    "Open record types with multiple labels have the same spacing in both formatters"
    (assertEqual "" "{a :: b, c :: d | r}" (renderText $ Type.docFromRow openRecord3))
  , testGroup
    "Open record types with multiple labels in parens have the same spacing in both formatters"
    [ testCase
      "single line"
      (assertEqual "" "({a :: b, c :: d | r})" (renderText $ Variations.singleLine $ Type.doc $ Type.TypeParens $ Type.TypeRow openRecord3))
    , testCase
      "multi line"
      (assertEqual "" "({a :: b, c :: d | r})" (renderText $ Variations.multiLine $ Type.doc $ Type.TypeParens $ Type.TypeRow openRecord3))
    ]
  ]

rowTests :: [TestTree]
rowTests =
  [ testCase
    "Open row types with one label have the same spacing in both formatters"
    (assertEqual "" "(a :: b | r)" (renderText $ Type.docFromRow openRow1))
  , testGroup
    "Open row types with one label in parens have the same spacing in both formatters"
    [ testCase
      "single line"
      (assertEqual "" "((a :: b | r))" (renderText $ Variations.singleLine $ Type.doc $ Type.TypeParens $ Type.TypeRow openRow1))
    , testCase
      "multi line"
      (assertEqual "" "((a :: b | r))" (renderText $ Variations.multiLine $ Type.doc $ Type.TypeParens $ Type.TypeRow openRow1))
    ]
  , testCase
    "Open row types with multiple labels have the same spacing in both formatters"
    (assertEqual "" "(a :: b, c :: d | r)" (renderText $ Type.docFromRow openRow2))
  , testGroup
    "Open row types with multiple labels in parens have the same spacing in both formatters"
    [ testCase
      "single line"
      (assertEqual "" "((a :: b, c :: d | r))" (renderText $ Variations.singleLine $ Type.doc $ Type.TypeParens $ Type.TypeRow openRow2))
    , testCase
      "multi line"
      (assertEqual "" "((a :: b, c :: d | r))" (renderText $ Variations.multiLine $ Type.doc $ Type.TypeParens $ Type.TypeRow openRow2))
    ]
  ]

emptyRecord :: Type.Row a
emptyRecord = Type.Row Type.RowBraces Nothing Type.Rowsed

openRecord1 :: Type.Row a
openRecord1 =
  Type.Row
    Type.RowBraces
    Nothing
    (Type.Rowpen $ Type.TypeVariable $ Type.Variable "foo")

openRecord2 :: Type.Row a
openRecord2 =
  Type.Row
    Type.RowBraces
    (Just $ Type.RowPair (Type.Label "a") (Type.TypeVariable $ Type.Variable "b") :| [])
    (Type.Rowpen $ Type.TypeVariable $ Type.Variable "r")

openRecord3 :: Type.Row a
openRecord3 =
  Type.Row
    Type.RowBraces
    (Just $ Type.RowPair (Type.Label "a") (Type.TypeVariable $ Type.Variable "b") :| [Type.RowPair (Type.Label "c") (Type.TypeVariable $ Type.Variable "d")])
    (Type.Rowpen $ Type.TypeVariable $ Type.Variable "r")

openRow1 :: Type.Row a
openRow1 =
  Type.Row
    Type.RowParens
    (Just $ Type.RowPair (Type.Label "a") (Type.TypeVariable $ Type.Variable "b") :| [])
    (Type.Rowpen $ Type.TypeVariable $ Type.Variable "r")

openRow2 :: Type.Row a
openRow2 =
  Type.Row
    Type.RowParens
    (Just $ Type.RowPair (Type.Label "a") (Type.TypeVariable $ Type.Variable "b") :| [Type.RowPair (Type.Label "c") (Type.TypeVariable $ Type.Variable "d")])
    (Type.Rowpen $ Type.TypeVariable $ Type.Variable "r")

renderText :: Doc a -> Text
renderText = renderStrict . layoutSmart defaultLayoutOptions
