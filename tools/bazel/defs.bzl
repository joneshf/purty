"""Helpers for working with bazel.
"""

def expand_template_action(ctx, substitutions):
    """Writes a templated file with the given version.

    We need a rule (instead of a macro) in order to use a template.
    It would be nice if `bazel_skylib` provided this.
    There's an issue tracking the request:
    https://github.com/bazelbuild/bazel-skylib/issues/191.

    Args:
        ctx: Analysis context.
        substitutions: Substitutions to make when expanding the template.
    """

    output = ctx.actions.declare_file(ctx.file.template.basename)

    ctx.actions.expand_template(
        output = output,
        substitutions = substitutions,
        template = ctx.file.template,
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

def _expand_template_impl(ctx):
    """Writes a templated file with the given version.

    We need a rule (instead of a macro) in order to use a template.
    It would be nice if `bazel_skylib` provided this.
    There's an issue tracking the request:
    https://github.com/bazelbuild/bazel-skylib/issues/191.

    Args:
        ctx: analysis context.
    """

    return expand_template_action(
        ctx = ctx,
        substitutions = ctx.attr.substitutions,
    )

expand_template = rule(
    attrs = {
        "substitutions": attr.string_dict(
            allow_empty = False,
            doc = "Substitutions to make when expanding the template.",
            mandatory = True,
        ),
        "template": attr.label(
            allow_single_file = True,
            doc = "The input template file.",
            mandatory = True,
        ),
    },
    doc = """Stamps the template file.

    This is a wrapper around actions.expand_template.
    """,
    implementation = _expand_template_impl,
    provides = [
        DefaultInfo,
    ],
)
