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


let rec token buf =
  match%sedlex buf with
  | letter, Star letter -> Printf.printf "Atom %s\n" (Sedlexing.Latin1.lexeme buf); token buf
  | eof -> print_endline "EOF"
  | _ -> failwith "Lexer error: Unexpected character"

let () =
  let lexbuf = Sedlexing.Latin1.from_string ex_atom2 in
  token lexbuf
;;

