"""Dependencies needed for `ormolu`.
"""

load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_file",
)

_ormolu_linux_sha = "55a3a15bd536ecaa36358a40618b558f6bc3e9695fcdf64a419205206ffd6145"
_ormolu_macos_sha = "80a475b7c107eb0deeaa50c73cc4f5467a250d0824d921bfb508581d5af2ca00"
_ormolu_windows_sha = "9ceb7c573666fbdeb5d79324375a5a7c2c15d0672e6732670705ab7340d3e23a"
_ormolu_version = "0.1.4.1"

def ormolu_dependencies():
    """Fetch dependencies for `ormolu`.

    Since `ormolu` is a tool we use (rather than an artifact we create),
    We want to keep its dependencies separate from ours.
    """

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
