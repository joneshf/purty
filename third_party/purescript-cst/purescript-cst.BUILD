load(
    "@rules_haskell//haskell:cabal.bzl",
    "haskell_cabal_library",
)

load(
    "@stackage//:packages.bzl",
    "packages",
)

haskell_cabal_library(
    name = "purescript-cst",
    deps = packages["purescript-cst"].deps,
    srcs = glob([
        "**",
    ]),
    tools = [
        "@happy//:happy",
    ],
    version = packages["purescript-cst"].version,
    visibility = [
        "//visibility:public",
    ],
)
