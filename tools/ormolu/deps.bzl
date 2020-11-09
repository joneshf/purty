"""Dependencies needed for `ormolu`.
"""

load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive",
    "http_file",
)

_ormolu_linux_sha = "ccb3089e2c89490ca9f3e196ce0cf2cafede6d572e35bb6aa225652c62b52df4"
_ormolu_macos_sha = "5609637a33c94309436f585e719cd5d230cdb1649d8e17b45690fbd1203c7b49"
_ormolu_windows_sha = "8db240bc6006e7fa80e70eaa46932f4fd9de1b1a781a8689d62a9a387e323191"
_ormolu_version = "0.1.3.0"

def ormolu_dependencies():
    """Fetch dependencies for `ormolu`.

    Since `ormolu` is a tool we use (rather than an artifact we create),
    We want to keep its dependencies separate from ours.
    """

    http_archive(
        build_file = "//tools/ormolu:ormolu.BUILD",
        name = "ormolu",
        sha256 = "8fb635d49812aac4b50a5dc04fbf07a3d6e586825a04606d6b2e81d817d7765f",
        strip_prefix = "ormolu-0.0.3.1",
        urls = [
            "https://hackage.haskell.org/package/ormolu-0.0.3.1/ormolu-0.0.3.1.tar.gz",
        ],
    )

    http_file(
        name = "ormolu_linux",
        executable = True,
        sha256 = _ormolu_linux_sha,
        urls = [
            "https://github.com/tweag/ormolu/releases/download/{ormolu_version}/ormolu-Linux".format(
                ormolu_version = _ormolu_version,
            ),
        ],
    )

    http_file(
        name = "ormolu_macos",
        executable = True,
        sha256 = _ormolu_macos_sha,
        urls = [
            "https://github.com/tweag/ormolu/releases/download/{ormolu_version}/ormolu-macOS".format(
                ormolu_version = _ormolu_version,
            ),
        ],
    )

    http_file(
        name = "ormolu_windows",
        executable = True,
        sha256 = _ormolu_windows_sha,
        urls = [
            "https://github.com/tweag/ormolu/releases/download/{ormolu_version}/ormolu-Windows".format(
                ormolu_version = _ormolu_version,
            ),
        ],
    )
