let letter = [%sedlex.regexp? 'a'..'z' | 'A'..'Z']
let arithmetic_op = [%sedlex.regexp? "+" | "-" | "*" | "/" | "%" ]
let boolean = [%sedlex.regexp? "#t" | "#f" ]
let digit = [%sedlex.regexp? '0'..'9']
let number = [%sedlex.regexp? Plus digit]
let ident = [%sedlex.regexp? letter, Star letter]

let lexeme (buf: Sedlexing.lexbuf) = Sedlexing.Utf8.lexeme buf

type token =
  QuotedList of token list |
  Symbol of string |
  Number of int |
  Boolean of bool |
  Plus | Minus | Divide | Multiply | Modulo |
  LT | LTE | GT | GTE | EQ | NEQ |
  AND | OR |
  LParen | RParen |
  Quote |
  EOF

(* S-expression type definition *)
type sexp = Atom of token | List of sexp list

(* Lexer exceptions *)
exception Lexer_exn of string
exception Unexpected_character of string
exception Parantheses_mismatch

(* Parser exceptions *)
exception Parser_exn of string
exception Incorrect_argument_count
exception Invalid_argument_types

(* Evaluation exceptions *)
exception Eval_exn of string
exception Not_function

let rec string_of_token token =
  match token with
  | Symbol s -> s
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
  | AND -> "&&"
  | OR -> "||"
  | Quote -> "'"
  | QuotedList tokens ->  
    "'("^(String.concat ", " (List.map (fun token -> string_of_token token) tokens))^")"

(* Scheme considers only #f to be false and anything else to be true *)
let boolean_of_string s =
  match s with
  | "#f" -> false
  | _ -> true 

let string_of_tokens tokens =
  "["^(String.concat ", " (List.map (fun token -> string_of_token token) tokens))^"]"

let rec string_of_sexp sexpr =
  match sexpr with
  | Atom x -> "Atom("^(string_of_token x)^")"
  | List xs -> "List("^(String.concat " " (List.map string_of_sexp xs))^")"

let rec tokenize buf tokens =
  match%sedlex buf with
  | white_space -> tokenize buf tokens
  | number -> tokenize buf (tokens@[Number (int_of_string (lexeme buf))])
  | boolean -> tokenize buf (tokens@[Boolean (boolean_of_string (lexeme buf))])
  | ident -> tokenize buf (tokens@[Symbol (lexeme buf)])
  | '+' -> tokenize buf (tokens@[Plus])
  | '-' -> tokenize buf (tokens@[Minus])
  | '/' -> tokenize buf (tokens@[Divide])
  | '*' -> tokenize buf (tokens@[Multiply])
  | '%' -> tokenize buf (tokens@[Modulo])
  | '(' -> tokenize buf (tokens@[LParen])
  | ')' -> tokenize buf (tokens@[RParen])
  | '=' -> tokenize buf (tokens@[EQ])
  | "\\=" -> tokenize buf (tokens@[NEQ])
  | '<' -> tokenize buf (tokens@[LT])
  | "<=" -> tokenize buf (tokens@[LTE])
  | '>' -> tokenize buf (tokens@[GT])
  | ">=" -> tokenize buf (tokens@[GTE])
  | "&&" -> tokenize buf (tokens@[AND])
  | "||" -> tokenize buf (tokens@[OR])
  | '\'' -> tokenize buf (tokens@[Quote])
  | eof -> tokens
  | any -> raise (Unexpected_character (lexeme buf))
  | _ -> raise (Unexpected_character "Unrecognized character")

(* Convert a list of tokens into an s-expression *)
let parse_to_sexp (tokens: token list) =
  let rec sexp_of_list tokens sexpr =
    match tokens with
    | [] -> raise Parantheses_mismatch
    | RParen::rem_tokens -> (List sexpr, rem_tokens)
    | Quote::LParen::rem_tokens -> 
      sexp_of_list rem_tokens (sexpr@[Atom Quote])
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
    | Quote::rem_tokens ->
      let (list_sexpr, rem_tokens) = sexp_of_list toks [] in
      aux rem_tokens (sexpr@[list_sexpr])
    | [x] -> [Atom x]
    | _ -> raise (Parser_exn "Invalid syntax") in
  let sexpr = aux tokens [] in
  List.hd sexpr
  (* FIXME: Remove the initial list variable to get rid of List.hd *)

let rec eval sexpr =

  let eval_binary_op op operands =
    if (List.length operands != 2) then raise Incorrect_argument_count
    else begin 
      match (List.map eval operands) with
      | [Number a; Number b] ->   
        begin
          match op with
          | Plus -> Number (a + b)
          | Minus -> Number (a - b)
          | Multiply -> Number (a * b)
          | Divide -> Number (a / b)
          | Modulo -> Number (a mod b)
          | EQ -> Boolean (a = b)
          | NEQ -> Boolean (a <> b)
          | LT -> Boolean (a < b)
          | LTE -> Boolean (a <= b)
          | GT -> Boolean (a > b)
          | GTE -> Boolean (a >= b)
          | _ -> raise (Parser_exn "Binary op not implemented")
        end 
      | [Boolean a; Boolean b] ->
        begin
          match op with
          | AND -> Boolean (a && b)
          | OR -> Boolean (a || b)
          | _ -> raise (Parser_exn "Binary op not implemented")
        end
      | _ -> raise Invalid_argument_types
    end in

  let eval_conditional op operands =
    (* Lazy evaluation *)
    let pred = eval (List.hd operands) in
    match pred with
    | Boolean b ->
      if b then (eval (List.hd (List.tl operands))) 
      else (eval (List.hd (List.tl (List.tl operands))))
    | _ -> raise (Parser_exn "Predicate is required for conditionals") in

  match sexpr with
  | Atom x -> x
  | List x ->
    begin
      match x with
      | (List _)::_ -> raise Not_function 
      | (Atom op)::operands ->
        begin
          match op with
          | Plus | Minus | Multiply | Divide | Modulo 
          | EQ | NEQ | LT | LTE | GT | GTE
          | AND | OR 
            -> eval_binary_op op operands
          | Symbol "if" -> eval_conditional op operands
          | Quote -> QuotedList (List.map eval operands) 
          | _ -> raise (Parser_exn ("Cannot parse operator: "^(string_of_token op)))
        end
      | [] -> raise (Parser_exn "List cannot be empty")
    end

let print_debug prelude s = print_endline ("DEBUG: "^prelude^s)

let interpret s =
  let _ = print_endline s in
  let lexbuf = Sedlexing.Utf8.from_string s in
  let tokens = tokenize lexbuf [] in
  let _ = print_debug "Tokens: " (string_of_tokens tokens) in
  let sexpr = parse_to_sexp tokens in
  let _ = print_debug "S-Expression: " (string_of_sexp sexpr) in
  let _ = print_debug "Result: " (string_of_token (eval sexpr)) in
  let result = eval sexpr in
  result

let () =
  try
    while true do
      print_string "scheme> ";
      print_endline (string_of_token (interpret (read_line ())));
    done
  with End_of_file -> ()
;;
