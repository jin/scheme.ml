open Eval

(* imports *)
let rec repl line_number =
  try
    begin
      print_string "scheme> ";
      print_string ((string_of_int line_number)^"> "^(interpret (read_line ()))^"\n");
      repl (line_number + 1)
    end
  with End_of_file -> ()

let has_filename = Array.length Sys.argv > 1

(* main function *)
(* all io operations are contained here *)
let () =
  if has_filename then
    let filename = Sys.argv.(1) in
    let src = open_in filename in
    try
      while true do
        let line = input_line src in
        let result = interpret line in
        let _ = print_endline result in
        flush stdout
      done
    with
    | End_of_file -> close_in src
    | e -> close_in_noerr src; raise e
  else repl 1;;
