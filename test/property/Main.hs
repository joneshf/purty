module Main where

import "rio" RIO

import "prettyprinter" Data.Text.Prettyprint.Doc             (Doc, layoutSmart)
import "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import "hedgehog" Hedgehog
    ( Gen
    , Group(Group, groupName, groupProperties)
    , Property
    , checkParallel
    , forAll
    , property
    , (===)
    )
import "purescript" Language.PureScript
    ( Literal(CharLiteral)
    )
import "rio" RIO.Text                                        (pack)
import "base" System.Exit                                    (exitFailure)

import qualified "hedgehog" Hedgehog.Gen

import "purty" Purty (defaultPrettyPrintConfig, layoutOptions)

import qualified "purty" Doc.Dynamic
import qualified "purty" Doc.Static

main :: IO ()
main = do
  passed <- checkParallel
    Group { groupName = "char can be parsed"
          , groupProperties =
            [ ("prop_dynamic_char_binder", prop_dynamic_char_binder)
            , ("prop_dynamic_char_expr", prop_dynamic_char_expr)
            , ("prop_static_char_binder", prop_static_char_binder)
            , ("prop_static_char_expr", prop_static_char_expr)
            ]
          }
  unless passed exitFailure

prop_dynamic_char_binder :: Property
prop_dynamic_char_binder = property $ do
  c <- forAll char
  renderText (Doc.Dynamic.fromLiteralBinder (CharLiteral c)) === pack (show c)

prop_dynamic_char_expr :: Property
prop_dynamic_char_expr = property $ do
  c <- forAll char
  renderText (Doc.Dynamic.fromLiteralExpr (CharLiteral c)) === pack (show c)

prop_static_char_binder :: Property
prop_static_char_binder = property $ do
  c <- forAll char
  renderText (Doc.Static.fromLiteralBinder (CharLiteral c)) === pack (show c)

prop_static_char_expr :: Property
prop_static_char_expr = property $ do
  c <- forAll char
  renderText (Doc.Static.fromLiteralExpr (CharLiteral c)) === pack (show c)

-- PureScript only supports the `\0` - `\xFFFF` range of characters.
--
-- For more information and the source of this decision, see:
-- https://github.com/purescript/purescript/blob/15d7330b3adb7f8d941052a7cfc4df075724d248/src/Language/PureScript/Parser/Lexer.hs#L263-L269
--
-- However, `AppVeyor` doesn't seem to work with the full range of characters.
-- We make a concession here and only check the ASCII characters.
-- We should fix this on `AppVeyor` instead of having a less than ideal test.
char :: Gen Char
char = Hedgehog.Gen.ascii

renderText :: Doc a -> Text
renderText = renderStrict . layoutSmart (layoutOptions defaultPrettyPrintConfig)
