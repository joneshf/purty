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
        build_file = "//tools/alex:alex.BUILD",
        name = "alex",
        sha256 = "d58e4d708b14ff332a8a8edad4fa8989cb6a9f518a7c6834e96281ac5f8ff232",
        strip_prefix = "alex-3.2.4",
        urls = [
            "https://hackage.haskell.org/package/alex-3.2.4/alex-3.2.4.tar.gz",
        ],
    )

    http_archive(
        build_file = "//tools/ormolu:ormolu.BUILD",
        name = "ormolu",
        sha256 = "8fb635d49812aac4b50a5dc04fbf07a3d6e586825a04606d6b2e81d817d7765f",
        strip_prefix = "ormolu-0.0.3.1",
        urls = [
            "https://hackage.haskell.org/package/ormolu-0.0.3.1/ormolu-0.0.3.1.tar.gz",
        ],
    )
