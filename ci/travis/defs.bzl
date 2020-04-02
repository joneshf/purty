"""Helpers for working with TravisCI.
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

    output = ctx.actions.declare_file("templated/{file}".format(
        file = ctx.file._template.path,
    ))

    ctx.actions.expand_template(
        output = output,
        substitutions = {
            "{{REPLACE_WITH_TAR}}": ctx.file.tar.path,
            "{{REPLACE_WITH_UPLOAD_FILENAME}}": ctx.attr.upload_filename,
            "{{REPLACE_WITH_VERSION}}": ctx.attr.version,
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

template_bintray = rule(
    attrs = {
        "_template": attr.label(
            allow_single_file = [
                ".json",
            ],
            default = "ci/travis/bintray.json",
            doc = "The input template file",
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
