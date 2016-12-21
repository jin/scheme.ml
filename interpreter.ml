open Util

let interpret (s: string) : string =
  Sedlexing.Utf8.from_string s
  |> Lexer.tokenize
  |> Parser.parse_to_sexp
  |> Eval.eval
  |> Types.string_of_value
