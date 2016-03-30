let ex_atom = "a";;
let ex_atom2 = "foo";;
let ex_empty_list = "()";;
let ex_list_one = "(a)";;
let ex_list_two = "(a b)";;
let ex_list_foo_bar = "(foo bar)";;
let ex_list_nested = "(a b (c) d)";;

let letter = [%sedlex.regexp? 'a'..'z' | 'A'..'Z']
let digit = [%sedlex.regexp? '0'..'0']
let number = [%sedlex.regexp? Plus digit]

let p s b = Printf.printf s (Sedlexing.Utf8.lexeme b);;

let rec token buf =
  match%sedlex buf with
  | white_space -> token buf
  | letter, Star letter -> p "Atom %s\n" buf; token buf
  | '(' -> p "LParen %s\n" buf; token buf
  | ')' -> p "RParen %s\n" buf; token buf
  | eof -> print_endline "EOF"
  | _ -> failwith "Lexer error: Unexpected character"

let () =
  let lexbuf = Sedlexing.Utf8.from_string ex_list_nested in
  token lexbuf
;;
