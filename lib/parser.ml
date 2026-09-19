open Lexer
open Ast

(* Parser internal state *)

let tokens : (token list) ref = ref []

(* Simple API for parser internal state *)

let peek () = List.hd !tokens

let drop ()  = 
  tokens := List.tl !tokens

let init (tks: token list) =
  tokens := tks

let raise_parse_error exp =
  let act = string_of_token (peek ()) in
  let msg = Printf.sprintf "PARSE ERROR: Expected %s but got %s." exp act in
  failwith msg

(* Consuming tokens of a specified shape. *)

let eat (tk:token) = 
  if peek () = tk then 
    drop ()
  else 
    raise_parse_error (string_of_token tk)

let eat_ident () : string =
  match peek () with 
  | TkIdent s -> drop (); s
  | _         -> raise_parse_error "Ident"

let eat_variable () : string =
  match peek () with
  | TkVar s -> drop (); s
  | _       -> raise_parse_error "Var" 

(* Parsing functions corresponding to each nonterminal. *)

let rec pCmd () : cmd =
  match peek () with
  | TkEnd ->  Skip
  | TkVar _ 
  | TkIdent _ -> Eval (pExp ())
  | TkDefine ->
      eat TkDefine;
      let f = eat_ident () in
      eat TkLParen;
      let args = pExpList () in
      eat TkRParen;
      eat TkEquals;
      let e = pExp () in
      eat TkEnd;
      Define (App (f, args), e)
  | _ -> raise_parse_error "Cmd"

and pExp () : exp =
  match peek () with
  | TkIdent _ ->
      let f = eat_ident () in
      eat TkLParen;
      let es = pExpList () in
      eat TkRParen;
      App (f, es)
  | TkVar _ -> Var (eat_variable ())
  | _ -> raise_parse_error "Exp"

and pExpList () : exp list =
  match peek () with
  | TkIdent _ 
  | TkVar _ -> 
      let e = pExp () in
      let es = pExpList' () in
      e::es
  | TkRParen -> []
  | _ -> raise_parse_error "ExpList"

and pExpList' () : exp list =
  match peek () with
  | TkRParen -> []
  | TkComma -> 
      eat TkComma;
      pExpList ()
  | _ -> raise_parse_error "ExpList'"


(* Parser API *)

let parse (s:string) : cmd =
  tokens := lex s;
  pCmd ()

(* Inline Tests *)

let%test _ =
  let expected = Skip
  in parse "" = expected

let%test _ =
  let expected = 
    Eval (App("Add", [App("Z",[]); App("S", [App("Z", [])])]))
  in
    parse "Add(Z(),S(Z()))" = expected

let%test _ =
  let expected = 
    Define(App("Add", [App("Z", []); Var "y"]), Var "y")
  in parse "def Add(Z(),y) = y" = expected