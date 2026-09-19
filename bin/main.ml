open Microbrew.Ast
open Microbrew.Parser
open Microbrew.Eval
  
(** 
    [repl env] implements a read-eval-print-loop (REPL), looping until 
    the end-of-file signal is given by e.g. the user pressing ctrl-d.  
    The evaluation part of the loop is with respect to the initial 
    store [env]. 
*)
let rec repl genv = 
  try 
    if Unix.isatty Unix.stdin then
      print_string "> ";
    let inp = read_line () in
    let c = parse inp in
    match c with
    | Skip -> 
        repl genv
    | Eval e ->
        let v = eval [] genv e in
        print_endline (string_of_exp v);
        repl genv
    | Define (head, body) ->
        let genv' = (head, body) :: genv in
        repl genv'
  with
  | End_of_file ->
      print_newline ()
  | Failure msg -> 
      print_endline msg;
      repl genv

(* This is effectively the entry point of the program. *)
let () = 
  print_endline "[µBrew]";
  repl []
  

  
