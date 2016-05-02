open Types

(* Null value in Scheme is represented as an empty list '() *)
let empty_list = List([Atom(Quote)])

(* Parser exceptions *)
exception Parser_exn of string
exception Incorrect_argument_count
exception Invalid_argument_types
exception Parentheses_mismatch

(* Convert a list of values into an s-expression *)
let parse_to_sexp (values: value list) =
  let rec sexp_of_list values sexpr =
    match values with
    | [] -> raise Parentheses_mismatch
    | RParen::rem_values -> (List sexpr, rem_values)
    | Quote::LParen::rem_values -> 
      begin
        match sexpr with
        | [] -> sexp_of_list rem_values (sexpr@[Atom Quote])
        | _ -> let (nested_list_sexpr, rem_values) = sexp_of_list rem_values [Atom Quote] in
          sexp_of_list rem_values (sexpr@[nested_list_sexpr])
      end
    | LParen::rem_values ->
      let (nested_list_sexpr, rem_values) = sexp_of_list rem_values [] in
      sexp_of_list rem_values (sexpr@[nested_list_sexpr])
    | x::rem_values -> sexp_of_list rem_values (sexpr@[Atom x])
  in
  let rec aux toks sexpr =
    match toks with
    | [] -> sexpr
    | LParen::rem_values ->
      let (list_sexpr, rem_values) = sexp_of_list rem_values [] in
      aux rem_values (sexpr@[list_sexpr])
    | RParen::_ -> raise Parentheses_mismatch
    | Quote::rem_values ->
      let (list_sexpr, rem_values) = sexp_of_list toks [] in
      aux rem_values (sexpr@[list_sexpr])
    | [x] -> [Atom x]
    | _ -> raise (Parser_exn "Invalid syntax") in
  let sexpr = aux values [] in
  List.hd sexpr
  (* FIXME: Remove the initial list variable to get rid of List.hd *)
