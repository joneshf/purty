"""Helpers for building `purty`.
"""

def _template_impl(ctx):
    """Writes a templated file with the given version.

    We need a rule (instead of a macro) in order to use a template.
    It would be nice if `bazel_skylib` provided this.
    There's an issue tracking the request:
    https://github.com/bazelbuild/bazel-skylib/issues/191.

    Args:
        ctx: analysis context.
    """

    output = ctx.actions.declare_file(ctx.file._template.path)

    ctx.actions.expand_template(
        output = output,
        substitutions = {
            "{{REPLACE_WITH_VERSION}}": version_purty,
        },
        template = ctx.file._template,
    )

    files = depset([
        output,
    ])

    runfiles = ctx.runfiles(
        transitive_files = files,
    )

    return [
        DefaultInfo(
            files = files,
            runfiles = runfiles,
        ),
    ]

template_version = rule(
    attrs = {
        "_template": attr.label(
            allow_single_file = True,
            default = "lib/Version.hs",
            doc = "The input Haskell template file",
        ),
    },
    doc = "Stamps the Haskell template file with the version",
    implementation = _template_impl,
    provides = [
        DefaultInfo,
    ],
)

version_purty = "6.1.2"
