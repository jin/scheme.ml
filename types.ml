type token =
  QuotedList of token list |
  Symbol of string |
  Keyword of keyword |
  Variable of string |
  Number of int |
  Boolean of bool |
  Plus | Minus | Divide | Multiply | Modulo |
  LT | LTE | GT | GTE | EQ | NEQ |
  AND | OR |
  LParen | RParen | (* TODO: Get rid of this *)
  Quote |
  EOF
and keyword =
  Car | Cdr | Cons | If | Cond

(* S-expression type definition *)
type sexp = Atom of token | List of sexp list

(* Debug functions *)

(* Scheme considers only #f to be false and anything else to be true *)
let boolean_of_string s =
  match s with
  | "#f" -> false
  | _ -> true 

exception Unknown_keyword

let string_of_keyword keyword =
  match keyword with
  | Car -> "car"
  | Cdr -> "cdr"
  | Cons -> "cons"
  | If -> "if"
  | Cond -> "cond"

let keyword_of_string s =
  match s with
  | "car" -> Keyword(Car)
  | "cdr" -> Keyword(Cdr)
  | "cons" -> Keyword(Cons)
  | "if" -> Keyword(If)
  | "cond" -> Keyword(Cond)
  | _ -> raise Unknown_keyword

let rec debug_string_of_token token =
  match token with
  | QuotedList tokens ->  
    "'("^(String.concat ", " (List.map (fun token -> string_of_token token) tokens))^")"
  | Symbol s -> "Symbol("^s^")"
  | Variable s -> "Variable("^s^")"
  | Keyword kw -> "Keyword("^(string_of_keyword kw)^")"
  | Number s -> "Number("^(string_of_int s)^")"
  | Quote -> "Quote"
  | _ -> string_of_token token 
and debug_string_of_tokens tokens =
  "["^(String.concat ", " (List.map (fun token -> debug_string_of_token token) tokens))^"]"
and string_of_token token =
  match token with
  | Symbol s -> s
  | Keyword kw -> string_of_keyword kw
  | Variable s -> s
  | Number s -> (string_of_int s)
  | Boolean s -> if s then "#t" else "#f"
  | LParen -> "LParen"
  | RParen -> "RParen"
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Divide -> "Divide"
  | Multiply -> "Multiply"
  | Modulo -> "Modulo"
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
  | QuotedList tokens ->  
    "("^(String.concat " " (List.map (fun token -> string_of_token token) tokens))^")"
and string_of_tokens tokens =
  "["^(String.concat " " (List.map (fun token -> string_of_token token) tokens))^"]"

let rec string_of_sexp sexpr =
  match sexpr with
  | Atom x -> "Atom("^(string_of_token x)^")"
  | List xs -> "List("^(String.concat " " (List.map string_of_sexp xs))^")"
