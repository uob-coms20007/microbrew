
(** An [exp] is an expression to be evaluated. *)
type exp =
  | Var of string
  | App of string * exp list

(* 
    String representations of ASTs. 

    Each of the AST types above has a corresponding function to convert it
    to a string.  Useful for printing out the AST for debugging purposes. 
*)

(** A [cmd] is a command to be executed in the REPL *)
type cmd =
  | Skip
  | Eval of exp
  | Define of exp * exp

(** [string_of_exp e] is the string representation of expression [e]. *)
let rec string_of_exp (e:exp) : string =
  match e with
  | Var s -> Printf.sprintf "%s" s
  | App (f, es) -> 
        Printf.sprintf "%s(%s)" f (string_of_exp_list es)

and string_of_exp_list (es:exp list) : string =
  match es with
  | [] -> ""
  | [e] -> string_of_exp e
  | e::es' -> string_of_exp e ^ ", " ^ string_of_exp_list es'

