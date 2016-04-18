let ex_atom = "a";;
let ex_atom2 = "foo";;
let ex_empty_list = "()";;
let ex_list_one = "(a)";;
let ex_list_two = "(a b)";;
let ex_list_foo_bar = "(foo bar)";;
let ex_list_nested = "(foo bar #t 123 12 (qux #f 40912) qaz)";;
let ex_list_arithmetic = "(- (+ (* 2 (/ 4 2)) (- 2 3)) (% 5 3))";;

let letter = [%sedlex.regexp? 'a'..'z' | 'A'..'Z']
let arithmetic_op = [%sedlex.regexp? "+" | "-" | "*" | "/" | "%" ]
let boolean = [%sedlex.regexp? "#t" | "#f" ]
let digit = [%sedlex.regexp? '0'..'9']
let number = [%sedlex.regexp? Plus digit]
let ident = [%sedlex.regexp? letter, Star letter]

let lexeme (buf: Sedlexing.lexbuf) = Sedlexing.Utf8.lexeme buf

type token = 
  Ident of string |
  Number of int |
  Boolean of bool |
  Plus | Minus | Divide | Multiply | Modulo |
  LParen | RParen |
  EOF

let string_of_token token =
  match token with
  | Ident s -> s
  | Number s -> "Number "^(string_of_int s)
  | Boolean s -> "Boolean "^(string_of_bool s)
  | LParen -> "LParen"
  | RParen -> "RParen"
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Divide -> "Divide"
  | Multiply -> "Multiply"
  | Modulo -> "Modulo"
  | EOF -> "\n"

let boolean_of_string s = 
  match s with
  | "#t" -> true
  | "#f" -> false
  | _ -> failwith "Not a boolean"

(* S-expression type definition *)
type sexp = Atom of token | List of sexp list

let head_of_sexp sexpr = 
  match sexpr with 
  | List (x::xs) -> x
  | _ -> failwith "Not a sexp list"

let tail_of_sexp sexpr =
  match sexpr with
  | List (x::xs) -> List (xs)
  | _ -> failwith "Not a sexp list"

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
  | ident -> tokenize buf (tokens@[Ident (lexeme buf)])
  | '+' -> tokenize buf (tokens@[Plus])
  | '-' -> tokenize buf (tokens@[Minus])
  | '/' -> tokenize buf (tokens@[Divide])
  | '*' -> tokenize buf (tokens@[Multiply])
  | '%' -> tokenize buf (tokens@[Modulo])
  | '(' -> tokenize buf (tokens@[LParen])
  | ')' -> tokenize buf (tokens@[RParen])
  | eof -> tokens 
  | _ -> failwith "Lexer error: Unexpected character"

(* Convert a list of tokens into an s-expression *)
let parse_to_sexp (tokens: token list) = 
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
  let sexpr = aux tokens [] in head_of_sexp (List sexpr)
  (* FIXME: Remove the initial list variable to get rid of head_of_sexp *)

let rec eval sexpr =

  let eval_binary_op operator a b =
    match operator with
    | Plus -> a + b
    | Minus -> a - b
    | Multiply -> a * b
    | Divide -> a / b
    | Modulo -> a mod b
    | _ -> failwith "TBI" in

  match sexpr with 
  | Atom x -> begin
    match x with
    | Number value -> value 
    | _ -> failwith "TBI"
    end
  | List x -> begin
      match (List.hd x, List.tl x) with  
      | (List _, _) -> failwith "First element of list should not be a List"
      | (Atom op, args) -> 
        let operands = List.map eval args in begin
          match op with 
          | Plus | Minus | Multiply | Divide | Modulo -> 
            eval_binary_op op (List.hd operands) (List.hd (List.tl operands))
          | _ -> failwith "TBI"
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

let () =
  let test_case = ex_list_arithmetic in
  let res = interpret test_case in
  let _ = print_endline (string_of_int res) in
  ()
;;
