"""Helpers for working with TravisCI.
"""

load(
    "//tools/bazel:defs.bzl",
    "expand_template_temporary_action",
)

def _template_impl(ctx):
    """Writes a templated file with the given version.

    This rule exists entirely because this issue has not been solved:
    https://github.com/bazelbuild/bazel/issues/5356. If we had a
    string_keyed_label_dict (or equivalent) we could change
    `//tools/bazel:defs.bzl.expand_template` to that, and remove this rule
    entirely. Until that exists, we need this rule so we can use label values.

    Args:
        ctx: analysis context.
    """

    substitutions = {
        "{{REPLACE_WITH_TAR}}": ctx.file.tar.path,
        "{{REPLACE_WITH_UPLOAD_FILENAME}}": ctx.attr.upload_filename,
        "{{REPLACE_WITH_VERSION}}": ctx.attr.version,
    }

    return expand_template_temporary_action(
        ctx = ctx,
        substitutions = substitutions,
    )

template_bintray = rule(
    attrs = {
        "template": attr.label(
            allow_single_file = [
                ".json",
            ],
            doc = "The input template file",
            mandatory = True,
        ),
        "tar": attr.label(
            allow_single_file = [
                ".tar.gz",
            ],
            doc = "The tarball to upload",
            mandatory = True,
        ),
        "upload_filename": attr.string(
            doc = "The filename that should be created on Bintray",
            mandatory = True,
        ),
        "version": attr.string(
            doc = "The version number",
            mandatory = True,
        ),
    },
    doc = "Stamps the template file with the version and other information",
    implementation = _template_impl,
    provides = [
        DefaultInfo,
    ],
)
