load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "io_bazel_rules_ocaml",
    urls = ["https://github.com/jin/rules_ocaml/archive/737e7fa92681a9dd0e1e99496476a2bdcc25f56a.tar.gz"],
    strip_prefix = "rules_ocaml-737e7fa92681a9dd0e1e99496476a2bdcc25f56a",
    sha256 = "245d8b21fb671d181d5402ddfe7c9d4f7a0d51d8b10adb26bf15b52b39f29620",
)

load("@io_bazel_rules_ocaml//ocaml:repo.bzl", "ocaml_repositories")
ocaml_repositories(
    opam_packages = {
        "sedlex": "1.99.4",
    },
)
