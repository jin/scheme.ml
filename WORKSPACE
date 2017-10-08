git_repository(
    name = "io_bazel_rules_ocaml",
    remote = "https://github.com/jin/rules_ocaml.git",
    commit = "7a0a6e5226af5f09eb6e3379b901d8f2ffdb8bbf",
)

load("@io_bazel_rules_ocaml//ocaml:repo.bzl", "ocaml_repositories")
ocaml_repositories(
    opam_packages = {
        # Put your OPAM dependencies here
        "sedlex": "1.99.4",
        "lwt": "3.1.0",
        "yojson": "1.4.0",
    },
)
