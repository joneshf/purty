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
  Left error -> Left ("Error decoding contents" <> displayShow error)
  Right decoded -> case Language.PureScript.CST.Parser.parse decoded of
    Left error' ->
      Left
        ( "Error parsing contents:"
            <> foldMap
              ( \error ->
                  newline
                    <> indentation
                    <> fromString (Language.PureScript.CST.Errors.prettyPrintError error)
              )
              error'
        )
    Right parsed -> Right parsed
