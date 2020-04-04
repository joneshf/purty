"""Helpers for golden tests.
"""

load(
    "@bazel_skylib//lib:paths.bzl",
    "paths",
)

load(
    "@bazel_skylib//rules:copy_file.bzl",
    "copy_file",
)

load(
    "@bazel_skylib//rules:diff_test.bzl",
    "diff_test",
)

def _golden_test(golden_file, input_file):
    """Runs `purty` on a PureScript file and compares the output.

    Args:
        golden_file: Golden PureScript file to compare.
        input_file: Input PureScript file to format.
    """

    formatted_file = paths.replace_extension(input_file, ".formatted.purs")
    formatted_name = "formatted/{input_file}".format(
        input_file = input_file,
    )
    native.genrule(
        cmd = "$(location //:purty-binary) $(location {input_file}) > $@".format(
            input_file = input_file,
        ),
        name = formatted_name,
        outs = [
            formatted_file,
        ],
        srcs = [
            input_file
        ],
        tools = [
            "//:purty-binary",
        ],
    )

    diff_name = "diff/{input_file}".format(
        input_file = input_file,
    )
    diff_test(
        file1 = golden_file,
        file2 = formatted_file,
        name = diff_name,
        tags = [
            "golden",
        ],
    )

def _golden_usage(input_file):
    """Validates a PureScript file is used in a golden test.

    Args:
        input_file: The PureScript file to validate.
    """

    usage_file = paths.replace_extension(input_file, ".usage.purs")
    name = "usage/{input_file}".format(
        input_file = input_file,
    )

    copy_file(
        name = name,
        out = usage_file,
        src = input_file,
    )

def golden_tests(srcs):
    """Runs `purty` on a collection of PureScript files and compare the output.

    Args:
        srcs: List of PureScript files to compare.
    """

    for src in srcs:
        filename = paths.basename(src)
        golden_file = paths.join("files", "formatted", filename)
        _golden_test(
            golden_file = golden_file,
            input_file = src,
        )

def golden_usages(srcs):
    """Validates a list of PureScript files are used in golden tests.

    Args:
        srcs: List of PureScript files to validate.
    """

    for src in srcs:
        filename = paths.basename(src)
        input_file = paths.join("files", "original", filename)
        _golden_usage(
            input_file = input_file,
        )
