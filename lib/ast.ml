open Option.Syntax

(** An [exp] is an expression to be evaluated. *)
type exp =
  | Var of string
  | App of string * exp list

(** 
  A substitution [subst] is just a variable name/expression association 
  list in which no variable appears twice.  This invariant is not enforced
  in the type.
*)
type subst = (string * exp) list

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

(**
  If [e1] matches [e2] then [pmatch e1 e2] returns [Some s], 
  where [s] is the induced substitution, and otherwise [None].

  Assumes that [e2] is linear, i.e. each variable name occurs 
  at most once.
*)
let rec pmatch (e1:exp) (e2:exp) : subst option =
  match e1, e2 with
  | _, Var x -> Some [(x, e1)]
  | App(f,e1s), App(g,e2s) when f = g -> pmatch_list e1s e2s
  | _, _ -> None

and pmatch_list (es1:exp list) (es2:exp list) : subst option =
  match es1, es2 with
  | [], [] -> Some []
  | e1::es1', e2::es2' ->
      let* ss1 = pmatch e1 e2 in
      let* ss2 = pmatch_list es1' es2' in
      Some (ss1 @ ss2)
  | _, _ -> None