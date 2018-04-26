module Main where

import "rio" RIO

import "prettyprinter" Data.Text.Prettyprint.Doc
    ( Doc
    , layoutCompact
    )
import "prettyprinter" Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import "hedgehog" Hedgehog
    ( Gen
    , Group(Group)
    , Property
    , checkParallel
    , forAll
    , property
    , (===)
    )

import qualified "hedgehog" Hedgehog.Gen
import qualified "hedgehog" Hedgehog.Range
import qualified "purescript" Language.PureScript

import "purty" Doc (fromFixity)

main :: IO ()
main = void $ checkParallel $ Group "Doc" [("borken test", prop_broken)]

prop_broken :: Property
prop_broken = property $ do
  fixity1 <- forAll fixity
  fixity2 <- forAll fixity
  renderText (fromFixity fixity1) === renderText (fromFixity fixity2)

associativity :: Gen Language.PureScript.Associativity
associativity =
  Hedgehog.Gen.element
    [ Language.PureScript.Infixl
    , Language.PureScript.Infix
    , Language.PureScript.Infixr
    ]

fixity :: Gen Language.PureScript.Fixity
fixity = do
  assoc <- associativity
  prec <- Hedgehog.Gen.integral (Hedgehog.Range.linear 10 0)
  pure (Language.PureScript.Fixity assoc prec)

renderText :: Doc a -> Text
renderText = renderStrict . layoutCompact
