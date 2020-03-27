load(
    "@rules_haskell//haskell:cabal.bzl",
    "haskell_cabal_binary",
    "haskell_cabal_library",
)

load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_toolchain_library",
)

haskell_cabal_binary(
    deps = [
        ":base",
        ":ormolu-library",
        ":text",
        "@stackage//:ghc-lib-parser",
        "@stackage//:gitrev",
        "@stackage//:optparse-applicative",
    ],
    name = "ormolu",
    srcs = glob([
        "**",
    ]),
    verbose = True,
    visibility = [
        "//visibility:public",
    ],
)

haskell_cabal_library(
    deps = [
        ":base",
        ":bytestring",
        ":containers",
        ":mtl",
        ":text",
        "@stackage//:dlist",
        "@stackage//:exceptions",
        "@stackage//:ghc-lib-parser",
        "@stackage//:syb",
    ],
    haddock = False,
    name = "ormolu-library",
    package_name = "ormolu",
    srcs = glob([
        "**",
    ]),
    verbose = True,
    version = "0.0.3.1",
    visibility = [
        "//visibility:public",
    ],
)

haskell_toolchain_library(
    name = "base",
)

haskell_toolchain_library(
    name = "bytestring",
)

haskell_toolchain_library(
    name = "containers",
)

haskell_toolchain_library(
    name = "mtl",
)

haskell_toolchain_library(
    name = "text",
)
