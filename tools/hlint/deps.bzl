"""Dependencies needed for `hlint`.
"""

load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive",
)

_hlint_version = "2.2.11"

def hlint_dependencies():
    """Fetch dependencies for `hlint`.

    Since `hlint` is a tool we use (rather than an artifact we create),
    We want to keep its dependencies separate from ours.
    """

    http_archive(
        build_file = "//tools/hlint:linux.BUILD",
        name = "hlint_linux",
        sha256 = "21ccdd7a79b20cfad4b690296e0f30b4439efb1b47a8ee4281c89be0303bd76b",
        strip_prefix = "hlint-{hlint_version}".format(
            hlint_version = _hlint_version,
        ),
        urls = [
            "https://github.com/ndmitchell/hlint/releases/download/v{hlint_version}/hlint-{hlint_version}-x86_64-linux.tar.gz".format(
                hlint_version = _hlint_version,
            ),
        ],
    )

    http_archive(
        build_file = "//tools/hlint:macos.BUILD",
        name = "hlint_macos",
        sha256 = "24ee6c423bf7fa491c7451b5881a3d9f2ca276c8808a5c42c0506be0842e0823",
        strip_prefix = "hlint-{hlint_version}".format(
            hlint_version = _hlint_version,
        ),
        urls = [
            "https://github.com/ndmitchell/hlint/releases/download/v{hlint_version}/hlint-{hlint_version}-x86_64-osx.tar.gz".format(
                hlint_version = _hlint_version,
            ),
        ],
    )

    http_archive(
        build_file = "//tools/hlint:windows.BUILD",
        name = "hlint_windows",
        sha256 = "3f71983ef0c89d40f1e3cab2f4a61b44626ba60babdf3f16101c18c71fb0cf6e",
        strip_prefix = "hlint-{hlint_version}".format(
            hlint_version = _hlint_version,
        ),
        urls = [
            "https://github.com/ndmitchell/hlint/releases/download/v{hlint_version}/hlint-{hlint_version}-x86_64-windows.zip".format(
                hlint_version = _hlint_version,
            ),
        ],
    )
