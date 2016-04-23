open Types

(* Null value in Scheme is represented as an empty list '() *)
let empty_list = List([Atom(Quote)])

(* Parser exceptions *)
exception Parser_exn of string
exception Incorrect_argument_count
exception Invalid_argument_types
exception Parantheses_mismatch

(* Convert a list of tokens into an s-expression *)
let parse_to_sexp (tokens: token list) =
  let rec sexp_of_list tokens sexpr =
    match tokens with
    | [] -> raise Parantheses_mismatch
    | RParen::rem_tokens -> (List sexpr, rem_tokens)
    | Quote::LParen::rem_tokens -> 
      begin
        match sexpr with
        | [] -> sexp_of_list rem_tokens (sexpr@[Atom Quote])
        | _ -> let (nested_list_sexpr, rem_tokens) = sexp_of_list rem_tokens [Atom Quote] in
          sexp_of_list rem_tokens (sexpr@[nested_list_sexpr])
      end
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
