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
let ident = [%sedlex.regexp? letter, Star letter]

let string_of_buf (buf: Sedlexing.lexbuf) = Sedlexing.Utf8.lexeme buf

type token = 
  Ident of string |
  Number of string |
  Boolean of string |
  LParen |
  RParen |
  EOF

let string_of_token token =
  match token with
  | Ident s -> s
  | Number s -> "Number "^s
  | Boolean s -> "Boolean "^s 
  | LParen -> "LParen"
  | RParen -> "RParen"
  | EOF -> "\n"

type sexp = Atom of token | List of sexp list

let string_of_tokens tokens = 
  "["^
    (String.concat 
       ", " 
       (List.map (fun token -> string_of_token token) tokens)
    )
  ^"]"

let rec string_of_sexp sexpr = 
  match sexpr with 
  | Atom x -> string_of_token x
  | List xs -> "("^(String.concat " " (List.map string_of_sexp xs))^")"

let rec tokenize buf tokens =
  match%sedlex buf with
  | white_space -> tokenize buf tokens
  | number -> tokenize buf (tokens@[Number (string_of_buf buf)])
  | boolean -> tokenize buf (tokens@[Boolean (string_of_buf buf)])
  | ident -> tokenize buf (tokens@[Ident (string_of_buf buf)])
  | '(' ->  tokenize buf (tokens@[LParen])
  | ')' ->  tokenize buf (tokens@[RParen])
  | eof -> tokens 
  | _ -> failwith "Lexer error: Unexpected character"

(* Convert a list of tokens into an s-expression *)
let sexp_of_tokens (tokens: token list) = 
  let rec sexp_of_list tokens sexpr =
    match tokens with 
    | [] -> (List sexpr, [])
    | RParen::rem_tokens -> (List sexpr, rem_tokens) 
    | LParen::rem_tokens -> 
      let (nested_list_sexpr, rem_tokens) = sexp_of_list rem_tokens [] in
      sexp_of_list rem_tokens (sexpr@[nested_list_sexpr])
    | x::rem_tokens -> sexp_of_list rem_tokens (sexpr@[Atom x])
  in
  let rec aux toks sexpr =
    match toks with 
    | [] -> sexpr
    | LParen::rem_tokens -> 
      let (list_sexpr, rem_tokens) = sexp_of_list rem_tokens [] in
      aux rem_tokens (sexpr@[list_sexpr])
    | _ -> sexpr in
  let sexpr = aux tokens [] in List sexpr


let () =
  let lexbuf = Sedlexing.Utf8.from_string ex_list_nested in
  let tokens = tokenize lexbuf [] in
  let _ = print_endline (string_of_tokens tokens) in
  let sexpr = sexp_of_tokens tokens in
  let _ = print_endline (string_of_sexp sexpr) in
  ()
;;
