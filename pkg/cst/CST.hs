{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module CST
  ( parse,
  )
where

import qualified "purescript-cst" Language.PureScript.CST.Errors
import qualified "purescript-cst" Language.PureScript.CST.Parser
import qualified "purescript-cst" Language.PureScript.CST.Types
import "rio" RIO hiding (error)

indentation :: Utf8Builder
indentation = "    "

newline :: Utf8Builder
newline = "\n"

parse ::
  LByteString ->
  Either Utf8Builder (Language.PureScript.CST.Types.Module ())
parse contents = case decodeUtf8' (toStrictBytes contents) of
  Left error -> Left (renderUnicodeException error)
  Right decoded -> parseText decoded

parseText ::
  Text ->
  Either Utf8Builder (Language.PureScript.CST.Types.Module ())
parseText decoded = case snd (Language.PureScript.CST.Parser.parse decoded) of
  Left error -> Left (renderParserErrors error)
  Right parsed -> Right parsed

renderParserError ::
  Language.PureScript.CST.Errors.ParserError ->
  Utf8Builder
renderParserError error =
  newline
    <> indentation
    <> fromString (Language.PureScript.CST.Errors.prettyPrintError error)

renderParserErrors ::
  NonEmpty Language.PureScript.CST.Errors.ParserError ->
  Utf8Builder
renderParserErrors error' = "Error parsing contents:" <> foldMap renderParserError error'

renderUnicodeException ::
  UnicodeException ->
  Utf8Builder
renderUnicodeException error = "Error decoding contents" <> displayShow error
