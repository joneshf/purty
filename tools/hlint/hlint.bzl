"""Macros for working with hlint.
"""

load(
    "@bazel_skylib//rules:native_binary.bzl",
    "native_test",
)

def hlint_test(srcs):
    """Generates a test rule for each Haskell source file.

    Args:
        srcs: Haskell source files to run hlint on.
    """

    for src in srcs:
        location = "$(location {src})".format(
            src = src,
        )
        name = "lint-hlint-{src}".format(
            src = src,
        )

        native_test(
            args = [
                location,
            ],
            data = [
                ".hlint.yaml",
                "//tools/hlint:hlint",
                src,
            ],
            name = name,
            out = name,
            src = "//tools/hlint:hlint",
            tags = [
                "hlint",
                "lint",
            ],
        )
