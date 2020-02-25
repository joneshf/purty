{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Version
  ( version,
  )
where

import qualified "file-embed" Data.FileEmbed
import "rio" RIO

version :: Builder
version = $(Data.FileEmbed.embedStringFile "version/purty")
