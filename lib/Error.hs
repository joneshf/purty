{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
module Error
  ( Error
  , format
  , message
  , new
  , wrap
  ) where

import "rio" RIO

import qualified "base" GHC.Stack

data Error
  = Error CallStack Utf8Builder

format :: HasCallStack => Error -> Utf8Builder
format error' = case error' of
  Error callStack message' ->
    message'
      <> foldMap
        (\(value, srcLoc) ->
          newline
            <> indentation
            <> fromString value
            <> " called from "
            <> fromString (GHC.Stack.prettySrcLoc srcLoc)
        )
        ( GHC.Stack.getCallStack GHC.Stack.callStack
          <> GHC.Stack.getCallStack callStack
        )

indentation :: Utf8Builder
indentation = "    "

message :: Error -> Utf8Builder
message error' = case error' of
  Error _ message' -> message'

new :: HasCallStack => Utf8Builder -> Error
new = Error GHC.Stack.callStack

newline :: Utf8Builder
newline = "\n"

wrap :: HasCallStack => Utf8Builder -> Error -> Error
wrap message'' error' = case error' of
  Error callStack message' ->
    Error
      ( GHC.Stack.fromCallSiteList
        $ GHC.Stack.getCallStack GHC.Stack.callStack
        <> GHC.Stack.getCallStack callStack
      )
      (message'' <> ": " <> message')
