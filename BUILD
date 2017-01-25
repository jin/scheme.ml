load("@io_bazel_rules_ocaml//ocaml:ocaml.bzl", "ocaml_native_binary", "ocaml_bytecode_binary")

filegroup(
    name = "srcs",
    srcs = [
        "eval.ml",
        "lexer.ml",
        "parser.ml",
        "scheme.ml",
        "types.ml",
        "interpreter.ml",
        "util.ml",
    ],
)

ocaml_bytecode_binary(
    name = "scheme-bytecode",
    srcs = [":srcs"],
    src_root = "scheme.ml",
    opam_packages = ["sedlex"],
)

ocaml_native_binary(
    name = "scheme-native",
    srcs = [":srcs"],
    src_root = "scheme.ml",
    opam_packages = ["sedlex"],
)
