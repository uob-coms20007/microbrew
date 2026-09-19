open Ast

type clause = (exp list * exp) list

(** 
    An [store] is just a list of pairs [(x, v)] with [x] a variable and [v] an
    expression.  We require that [v] is moreover a value, but this is not
    enforced by the types. 
*)
type lenv = (string * exp) list
type genv = (string * clause) list

(** [raise_eval_error s] raises [Failure] with a runtime error message *)
let raise_eval_error s =
  let str = string_of_exp s in
  let msg = "RUNTIME ERROR: Evaluation undefined for " ^ str ^ "." in
  failwith msg

(** 
    [eval l g e] evaluates expression [e] in local env [l] and global env [g]:
    * if [e] is of shape [x] then replace [x] by its associated expression in [l]
    * if [e] is of shape [f(es)] then evaluate each expression in the list [es] and
      then match it against one of the clauses for [f] in [g], perform the substition
      induced by the matching and then evaluate the resulting expression.
*)
let rec eval lenv genv e =
  match e with
  | Var x -> 
      (match List.assoc_opt x lenv with
       | None -> raise_eval_error e
       | Some v -> v)
  | App (f,es) ->
      let vs = List.map (eval lenv genv) es in
      let e' = App (f,vs) in 
      match select_clause e' genv with
      | None -> e'
      | Some (lenv', body) -> eval (lenv' @ lenv) genv body
and select_clause actual genv : (subst * exp) option =
  match genv with
  | [] -> None
  | (head,body)::genv' -> 
      match pmatch actual head with
      | None -> select_clause actual genv'
      | Some l' -> Some (l', body)


