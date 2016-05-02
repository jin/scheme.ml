type value =
  QuotedList of value list |
  Symbol of string |
  Keyword of string |
  Variable of string |
  Number of int |
  Boolean of bool |
  String of string |
  Plus | Minus | Divide | Multiply | Modulo |
  LT | LTE | GT | GTE | EQ | NEQ |
  AND | OR |
  LParen | RParen | (* TODO: Get rid of this *)
  Quote |
  EOF

(* S-expression type definition *)
type sexp = Atom of value | List of sexp list

(* Debug functions *)

(* Scheme considers only #f to be false and anything else to be true *)
let boolean_of_string s =
  match s with
  | "#f" -> false
  | _ -> true 

exception Unknown_keyword

let rec debug_string_of_value value =
  match value with
  | QuotedList values ->  
    "'("^(String.concat ", " (List.map (fun value -> string_of_value value) values))^")"
  | Symbol s -> "Symbol("^s^")"
  | Variable s -> "Variable("^s^")"
  | Keyword kw -> "Keyword("^kw^")"
  | Number s -> "Number("^(string_of_int s)^")"
  | Quote -> "Quote"
  | String s -> "String("^s^")"
  | _ -> string_of_value value 
and debug_string_of_values values =
  "["^(String.concat ", " (List.map (fun value -> debug_string_of_value value) values))^"]"
and string_of_value value =
  match value with
  | Symbol s -> s
  | Keyword kw -> kw
  | Variable s -> s
  | Number s -> (string_of_int s)
  | Boolean s -> if s then "#t" else "#f"
  | String s -> s
  | LParen -> "LParen"
  | RParen -> "RParen"
  | Plus -> "+"
  | Minus -> "-"
  | Divide -> "/"
  | Multiply -> "*"
  | Modulo -> "%"
  | EOF -> "\n"
  | LT -> "<"
  | LTE -> "<="
  | GT -> ">"
  | GTE -> ">="
  | EQ -> "="
  | NEQ -> "/="
  | AND -> "and"
  | OR -> "or"
  | Quote -> "'"
  | QuotedList values ->  
    "("^(String.concat " " (List.map (fun value -> string_of_value value) values))^")"
and string_of_values values =
  "["^(String.concat " " (List.map (fun value -> string_of_value value) values))^"]"

let rec string_of_sexp sexpr =
  match sexpr with
  | Atom x -> "Atom("^(string_of_value x)^")"
  | List xs -> "List("^(String.concat " " (List.map string_of_sexp xs))^")"
