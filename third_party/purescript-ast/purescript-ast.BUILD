load(
    "@rules_haskell//haskell:cabal.bzl",
    "haskell_cabal_library",
)

load(
    "@stackage//:packages.bzl",
    "packages",
)

haskell_cabal_library(
    name = "purescript-ast",
    deps = packages["purescript-ast"].deps,
    srcs = glob([
        "**",
    ]),
    version = packages["purescript-ast"].version,
    visibility = [
        "//visibility:public",
    ],
)
