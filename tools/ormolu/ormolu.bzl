"""Macros for working with ormolu.
"""

load(
    "@bazel_skylib//rules:native_binary.bzl",
    "native_test",
)

load(
    "@bazel_skylib//lib:shell.bzl",
    "shell",
)

def ormolu_format(name, srcs):
    """Generates a binary rule to format all Haskell source files.

    Args:
        name: Name of the target.
        srcs: Haskell source files to run ormolu on.
    """

    haskell_srcs = []

    for src in srcs:
        haskell_src = "$(location {src})".format(
            src = src,
        )
        haskell_srcs.append(shell.quote(haskell_src))

    native.sh_binary(
        args = [
            "--ormolu $(location //tools/ormolu:ormolu)",
        ] + haskell_srcs,
        data = [
            "//tools/ormolu:ormolu",
        ] + srcs,
        name = name,
        srcs = [
            "//tools/ormolu:format.sh",
        ],
        tags = [
            "format",
            "ormolu",
        ],
    )

def ormolu_test(srcs):
    """Generates a test rule for each Haskell source file.

    Args:
        srcs: Haskell source files to run ormolu on.
    """

    for src in srcs:
        location = "$(location {src})".format(
            src = src,
        )
        name = "lint-ormolu/{src}".format(
            src = src,
        )

        native_test(
            args = [
                "--mode check",
                location,
            ],
            data = [
                "//tools/ormolu:ormolu",
                src,
            ],
            name = name,
            out = name,
            src = "//tools/ormolu:ormolu",
            tags = [
                "ormolu",
                "lint",
            ],
        )
