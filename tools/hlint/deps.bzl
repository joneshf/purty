"""Dependencies needed for `hlint`.
"""

load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive",
)

_hlint_linux_sha = "eaf42de9986be58205d1d8ce1e518ccf41b367efed10561a19a7a454f0d1db82"
_hlint_macos_sha = "43435624c833bce8d2ae01614d17b48724924c69f69916dd809d0683d6624069"
_hlint_version = "3.2.7"
_hlint_windows_sha = "8ba44ee40d169e35ea6acf017a81a289cfd311e5d8a509ff65ea8e65f2709ac6"

def hlint_dependencies():
    """Fetch dependencies for `hlint`.

    Since `hlint` is a tool we use (rather than an artifact we create),
    We want to keep its dependencies separate from ours.
    """

    http_archive(
        build_file = "//tools/hlint:linux.BUILD",
        name = "hlint_linux",
        sha256 = _hlint_linux_sha,
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
        sha256 = _hlint_macos_sha,
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
        sha256 = _hlint_windows_sha,
        strip_prefix = "hlint-{hlint_version}".format(
            hlint_version = _hlint_version,
        ),
        urls = [
            "https://github.com/ndmitchell/hlint/releases/download/v{hlint_version}/hlint-{hlint_version}-x86_64-windows.zip".format(
                hlint_version = _hlint_version,
            ),
        ],
    )
