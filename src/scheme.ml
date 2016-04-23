open Parser
open Types

(* Evaluation exceptions *)
exception Eval_exn of string
exception Not_function


let rec eval (sexpr: sexp) : token =

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
          | _ -> raise (Parser_exn "Type error")
        end 
      | [Boolean a; Boolean b] ->
        begin
          match op with
          | AND -> Boolean (a && b)
          | OR -> Boolean (a || b)
          | _ -> raise (Parser_exn "Type error")
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

  let eval_car op operands =
    match operands with 
    | x::xs -> 
      begin
        match eval x with
        | QuotedList(y::ys) -> y
        | _ -> raise Incorrect_argument_count
      end
    | _ -> raise Incorrect_argument_count in

  let eval_cdr op operands =
    match operands with 
    | x::xs -> 
      begin
        match eval x with
        | QuotedList(y::ys) -> QuotedList(ys)
        | _ -> raise Incorrect_argument_count
      end
    | _ -> raise Incorrect_argument_count in

  let eval_cons op operands =
    match operands with
    (* atom or list -> empty list *)
    | [head; List([Atom(Quote)])] ->
      QuotedList([eval head])
    | [head; List(Atom(Quote)::tail)] -> 
      QuotedList((eval head)::(List.map eval tail))
    | [head; tail] -> 
      begin
        match eval tail with
        | QuotedList(x) -> QuotedList((eval head)::x)
        | _ -> failwith "TBI"
      end
    | _ -> raise Incorrect_argument_count in

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
          | AND | OR -> eval_binary_op op operands
          | Keyword "if" -> eval_conditional op operands
          | Keyword "car" -> eval_car op operands
          | Keyword "cdr" -> eval_cdr op operands
          | Keyword "cons" -> eval_cons op operands
          | Quote -> QuotedList (List.map eval operands) 
          | _ -> raise (Parser_exn ("Cannot parse operator: "^(string_of_token op)))
        end
      | [] -> raise (Parser_exn "List cannot be empty")
    end

let print_debug prelude s = print_endline ("DEBUG: "^prelude^s)

let interpret s =
  (* let _ = print_endline s in *)
  let lexbuf = Sedlexing.Utf8.from_string s in
  let tokens = tokenize lexbuf [] in
  (* let _ = print_debug "Tokens: " (debug_string_of_tokens tokens) in *)
  let sexpr = parse_to_sexp tokens in
  (* let _ = print_debug "S-Expression: " (string_of_sexp sexpr) in *)
  (* let _ = print_debug "Result: " (string_of_token (eval sexpr)) in *)
  let result = eval sexpr in
  result

let () =
  let line_number = ref 1 in
  try
    while true do
      print_string "scheme> ";
      print_endline ((string_of_int !line_number)^"> "^(string_of_token (interpret (read_line ()))));
      line_number := 1 + !line_number;
    done
  with End_of_file -> ()
;;
