open Types

(* Regular expressions *)
let letter = [%sedlex.regexp? 'a'..'z' | 'A'..'Z']
let arithmetic_op = [%sedlex.regexp? "+" | "-" | "*" | "/" | "%" ]
let boolean = [%sedlex.regexp? "#t" | "#f" ]
let digit = [%sedlex.regexp? '0'..'9']
let number = [%sedlex.regexp? Opt '-', Plus digit]
let variable = [%sedlex.regexp? Plus letter]
let symbol = [%sedlex.regexp? '\'', Plus letter]
let keyword = [%sedlex.regexp? "if" | "car" | "cdr" | "cons" | "quote" ]
let some_string = [%sedlex.regexp? '"', Star(Compl ('"') | "\\\""), '"']

let lexeme (buf: Sedlexing.lexbuf) = Sedlexing.Utf8.lexeme buf

(* Lexer exceptions *)
exception Lexer_exn of string
exception Lexer_failure
exception Unexpected_character of string

(* Lexing function *)
let tokenize (buf: Sedlexing.lexbuf) : value list =
  let rec aux buf tokens =
    match%sedlex buf with
    | white_space -> aux buf tokens
    | '+' -> aux buf (tokens@[Plus])
    | '-' -> aux buf (tokens@[Minus])
    | '/' -> aux buf (tokens@[Divide])
    | '*' -> aux buf (tokens@[Multiply])
    | '%' -> aux buf (tokens@[Modulo])
    | '(' -> aux buf (tokens@[LParen])
    | ')' -> aux buf (tokens@[RParen])
    | '=' -> aux buf (tokens@[EQ])
    | "\\=" -> aux buf (tokens@[NEQ])
    | '<' -> aux buf (tokens@[LT])
    | "<=" -> aux buf (tokens@[LTE])
    | '>' -> aux buf (tokens@[GT])
    | ">=" -> aux buf (tokens@[GTE])
    | "and" -> aux buf (tokens@[AND])
    | "or" -> aux buf (tokens@[OR])
    | '\'' -> aux buf (tokens@[Quote])
    | some_string -> aux buf (tokens@[String (lexeme buf)])
    | number -> aux buf (tokens@[Number (int_of_string (lexeme buf))])
    | boolean -> aux buf (tokens@[Boolean (boolean_of_string (lexeme buf))])
    | symbol -> aux buf (tokens@[Symbol (lexeme buf)])
    | keyword -> aux buf (tokens@[Keyword (lexeme buf)])
    | variable -> aux buf (tokens@[Variable (lexeme buf)])
    | eof -> tokens
    | any -> raise (Unexpected_character (lexeme buf))
    | _ -> raise Lexer_failure in
  aux buf []
