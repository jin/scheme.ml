let ex_atom = "a";;
let ex_atom2 = "foo";;
let ex_empty_list = "()";;
let ex_list_one = "(a)";;
let ex_list_two = "(a b)";;
let ex_list_foo_bar = "(foo bar)";;
let ex_list_nested = "(foo bar #t 123 12 (qux #f 40912) qaz)";;

let letter = [%sedlex.regexp? 'a'..'z' | 'A'..'Z']
let boolean = [%sedlex.regexp? "#t" | "#f" ]
let digit = [%sedlex.regexp? '0'..'9']
let number = [%sedlex.regexp? Plus digit]

let atom = [%sedlex.regexp? letter, Star letter]

let p s b = Printf.printf s (Sedlexing.Utf8.lexeme b);;

let string_of_buf (buf: Sedlexing.lexbuf) = Sedlexing.Utf8.lexeme buf

let number_of_buf (buf: Sedlexing.lexbuf) = int_of_string (string_of_buf buf)

let boolean_of_buf (buf: Sedlexing.lexbuf) = match string_of_buf buf with
  | "#t" -> true 
  | "#f" -> false
  | _ -> failwith "Unable to parse boolean"

type token = 
  Ident of string |
  Number of int |
  Boolean of bool |
  LParen |
  RParen |
  EOF

let string_of_token token =
  match token with
  | Ident s -> s
  | Number s -> "Number "^(string_of_int s)
  | Boolean s -> if s then "Boolean #t" else "Boolean #f"
  | LParen -> "LParen"
  | RParen -> "LParen"
  | EOF -> "\n"

type sexp = Atom of token | List of sexp list

let string_of_tokens tokens = 
  "["^(String.concat ", " (List.map (fun token -> string_of_token token) tokens))^"]"

let rec tokenize buf tokens =
  match%sedlex buf with
  | white_space -> tokenize buf tokens
  | number -> tokenize buf (tokens@[Number (number_of_buf buf)])
  | boolean -> tokenize buf (tokens@[Boolean (boolean_of_buf buf)])
  | atom -> tokenize buf (tokens@[Ident (string_of_buf buf)])
  | '(' ->  tokenize buf (tokens@[LParen])
  | ')' ->  tokenize buf (tokens@[RParen])
  | eof -> tokens 
  | _ -> failwith "Lexer error: Unexpected character"

let to_sexp (tokens: token list) = 
  let rec aux toks sexpr =
    match toks with 
    | [] -> sexpr
    | _ -> sexpr in
  aux tokens []


let () =
  let lexbuf = Sedlexing.Utf8.from_string ex_list_nested in
  let tokens = tokenize lexbuf [] in
  let _ = print_endline (string_of_tokens tokens) in
  let sexpr = to_sexp tokens in
  ()
;;
