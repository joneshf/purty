"""Dependencies needed for `ormolu`.
"""

load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive",
)

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
