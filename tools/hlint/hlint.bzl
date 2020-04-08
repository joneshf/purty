"""Macros for working with hlint.
"""

load(
    "@bazel_skylib//rules:native_binary.bzl",
    "native_test",
)

def hlint_test(name, srcs, **kwargs):
    """Generates a test rule for each Haskell source file.
    Then places all of them into a single test suite.

    Args:
        name: Name of the test suite.
        srcs: Haskell source files to run hlint on.
        kwargs: Additional keyword arguments to pass to the `test_suite` rule.
    """

    kwarg_tags = kwargs.pop("tags", [])
    tags = kwarg_tags.append([
        "hlint",
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
                location,
            ],
            data = [
                "//:.hlint.yaml",
                "//tools/hlint:hlint",
                src,
            ],
            name = test_name,
            out = test_name,
            src = "//tools/hlint:hlint",
            tags = [
                "hlint",
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
