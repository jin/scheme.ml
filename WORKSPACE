# local_repository(
#     name = "io_bazel_rules_ocaml",
#     path = "/Users/jin/Code/rules_ocaml",
# )

git_repository(
    name = "io_bazel_rules_ocaml",
    remote = "https://github.com/jin/rules_ocaml.git",
    commit = "18685e1bc5ad22e425f502355087747a6638f51a",
)

load("@io_bazel_rules_ocaml//ocaml:ocaml.bzl", "ocaml_repositories")
ocaml_repositories()
