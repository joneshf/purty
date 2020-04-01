{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Version
  ( version,
  )
where

import "rio" RIO

version :: Builder
version = "{{REPLACE_WITH_VERSION}}"
