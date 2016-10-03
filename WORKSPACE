# local_repository(
#     name = "io_bazel_rules_ocaml",
#     path = "/Users/jin/Code/rules_ocaml",
# )

git_repository(
    name = "io_bazel_rules_ocaml",
    remote = "https://github.com/jin/rules_ocaml.git",
    commit = "de567d0e6653f9e8208a42bfe26b30d58e6beeaa",
)

load("@io_bazel_rules_ocaml//ocaml:ocaml.bzl", "ocaml_repositories")
ocaml_repositories()
