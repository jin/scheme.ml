load("@io_bazel_rules_ocaml//ocaml:ocaml.bzl", "ocaml_native_binary", "ocaml_bytecode_binary")

ocaml_bytecode_binary(
    name = "scheme-bytecode",
    src = "scheme.ml",
    opam_pkgs = ["sedlex"],
)

ocaml_native_binary(
    name = "scheme-native",
    src = "scheme.ml",
    opam_pkgs = ["sedlex"],
)
