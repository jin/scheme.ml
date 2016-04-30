open Types

let letter = [%sedlex.regexp? 'a'..'z' | 'A'..'Z']
let arithmetic_op = [%sedlex.regexp? "+" | "-" | "*" | "/" | "%" ]
let boolean = [%sedlex.regexp? "#t" | "#f" ]
let digit = [%sedlex.regexp? '0'..'9']
let number = [%sedlex.regexp? Opt '-', Plus digit]
let variable = [%sedlex.regexp? letter, Star letter]
let symbol = [%sedlex.regexp? '\'', letter, Star letter]
let keyword = [%sedlex.regexp? "if" | "car" | "cdr" | "cons" ]

let lexeme (buf: Sedlexing.lexbuf) = Sedlexing.Utf8.lexeme buf

(* Lexer exceptions *)
exception Lexer_exn of string
exception Lexer_failure
exception Unexpected_character of string

let rec tokenize buf tokens =
  match%sedlex buf with
  | white_space -> tokenize buf tokens
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
  | "and" -> tokenize buf (tokens@[AND])
  | "or" -> tokenize buf (tokens@[OR])
  | '\'' -> tokenize buf (tokens@[Quote])
  | number -> tokenize buf (tokens@[Number (int_of_string (lexeme buf))])
  | boolean -> tokenize buf (tokens@[Boolean (boolean_of_string (lexeme buf))])
  | symbol -> tokenize buf (tokens@[Symbol (lexeme buf)])
  | keyword -> tokenize buf (tokens@[keyword_of_string (lexeme buf)])
  | variable -> tokenize buf (tokens@[Variable (lexeme buf)])
  | eof -> tokens
  | any -> raise (Unexpected_character (lexeme buf))
  | _ -> raise Lexer_failure
