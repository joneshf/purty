module Main where

import "rio" RIO

import "prettyprinter" Data.Text.Prettyprint.Doc             (Doc, layoutSmart)
import "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import "purescript" Language.PureScript
    ( Type(ParensInType, REmpty, TypeApp)
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

import "purty" Purty (defaultPrettyPrintConfig, layoutOptions)

import qualified "purty" Doc.Dynamic
import qualified "purty" Doc.Static

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests =
  testGroup
    "Doc"
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
  where
  emptyRecord :: Language.PureScript.Type
  emptyRecord = TypeApp tyRecord REmpty

renderText :: Doc a -> Text
renderText = renderStrict . layoutSmart (layoutOptions defaultPrettyPrintConfig)
