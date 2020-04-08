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

def ormolu_test(name, srcs, **kwargs):
    """Generates a test rule for each Haskell source file.
    Then places all of them into a single test suite.

    Args:
        name: Name of the test suite.
        srcs: Haskell source files to run ormolu on.
        kwargs: Additional keyword arguments to pass to the `test_suite` rule.
    """

    kwarg_tags = kwargs.pop("tags", [])
    tags = kwarg_tags.append([
        "ormolu",
        "lint",
    ])
    tests = []

    for src in srcs:
        location = "$(location {src})".format(
            src = src,
        )
        test_name = "{name}/{src}".format(
            name = name,
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
            name = test_name,
            out = test_name,
            src = "//tools/ormolu:ormolu",
            tags = [
                "ormolu",
                "lint",
            ],
        )

        tests.append(test_name)

    native.test_suite(
        name = name,
        tests = tests,
        tags = tags,
        **kwargs,
    )
