let letter = [%sedlex.regexp? 'a'..'z' | 'A'..'Z']
let arithmetic_op = [%sedlex.regexp? "+" | "-" | "*" | "/" | "%" ]
let boolean = [%sedlex.regexp? "#t" | "#f" ]
let digit = [%sedlex.regexp? '0'..'9']
let number = [%sedlex.regexp? Plus digit]
let ident = [%sedlex.regexp? letter, Star letter]

let lexeme (buf: Sedlexing.lexbuf) = Sedlexing.Utf8.lexeme buf

type token =
  Symbol of string |
  Number of int |
  Boolean of bool |
  Plus | Minus | Divide | Multiply | Modulo |
  LT | LTE | GT | GTE | EQ | NEQ |
  LParen | RParen |
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

(* Evaluation exceptions *)
exception Eval_exn of string
exception Not_function

let string_of_token token =
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

let boolean_of_string s =
  match s with
  | "#t" -> true
  | "#f" -> false
  | _ -> false

let string_of_tokens tokens =
  "["^(String.concat ", " (List.map (fun token -> string_of_token token) tokens))^"]"

let rec string_of_sexp sexpr =
  match sexpr with
  | Atom x -> string_of_token x
  | List xs -> "("^(String.concat " " (List.map string_of_sexp xs))^")"

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
  | eof -> tokens
  | any -> raise (Unexpected_character (lexeme buf))
  | _ -> raise (Unexpected_character "Unrecognized character")

(* Convert a list of tokens into an s-expression *)
let parse_to_sexp (tokens: token list) =
  let rec sexp_of_list tokens sexpr =
    match tokens with
    | [] -> raise Parantheses_mismatch
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
    | [x] -> [Atom x]
    | _ -> raise (Parser_exn "Invalid syntax") in
  let sexpr = aux tokens [] in
  List.hd sexpr
  (* FIXME: Remove the initial list variable to get rid of List.hd *)

let rec eval sexpr =

  let eval_binary_op op operands =
    begin 
      match operands with
      | [Number a; Number b] ->   
        begin
          match (op, operands) with
          | (Plus, [Number a; Number b]) -> Number (a + b)
          | (Minus, [Number a; Number b]) -> Number (a - b)
          | (Multiply, [Number a; Number b]) -> Number (a * b)
          | (Divide, [Number a; Number b]) -> Number (a / b)
          | (Modulo, [Number a; Number b]) -> Number (a mod b)
          | (EQ, [Number a; Number b]) -> Boolean (a = b)
          | (NEQ, [Number a; Number b]) -> Boolean (a <> b)
          | (LT, [Number a; Number b]) -> Boolean (a < b)
          | (LTE, [Number a; Number b]) -> Boolean (a <= b)
          | (GT, [Number a; Number b]) -> Boolean (a > b)
          | (GTE, [Number a; Number b]) -> Boolean (a >= b)
          | _ -> raise (Parser_exn "Binary op not implemented")
        end 
      | _ -> raise Incorrect_argument_count
    end in

  match sexpr with
  | Atom x -> x
  | List x ->
    begin
      match (List.hd x, List.tl x) with
      | (List _, _) -> raise Not_function 
      | (Atom op, args) ->
        let operands = List.map eval args in
        begin
          match op with
          | Plus | Minus | Multiply | Divide | Modulo 
          | EQ | NEQ | LT | LTE | GT | GTE
            -> eval_binary_op op operands
          | _ -> raise (Parser_exn "TBI")
        end
    end

let interpret s =
  let lexbuf = Sedlexing.Utf8.from_string s in
  let tokens = tokenize lexbuf [] in
  (* let _ = print_endline (string_of_tokens tokens) in *)
  let sexpr = parse_to_sexp tokens in
  (* let _ = print_endline (string_of_sexp sexpr) in *)
  (* let _ = print_endline (string_of_int (eval sexpr)) in *)
  let result = eval sexpr in
  result

let read str = str

let () =
  try
    while true do
      print_string "scheme> ";
      print_endline (string_of_token (interpret (read_line ())));
    done
  with End_of_file -> ()
;;
