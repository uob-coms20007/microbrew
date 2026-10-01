open Lexer

(* Recogniser internal state *)

let tokens : (token list) ref = ref []

(* Simple API for recogniser internal state *)

let peek () = List.hd !tokens

let drop ()  = 
  tokens := List.tl !tokens

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

let rec pCmd () =
  match peek () with
  | TkEnd ->  ()
  | TkVar _ 
  | TkIdent _ -> 
      pExp ()
  | TkDefine ->
      eat TkDefine;
      let _ = eat_ident () in
      eat TkLParen;
      pExpList ();
      eat TkRParen;
      eat TkEquals;
      pExp ();
      eat TkEnd
  | _ -> raise_parse_error "Cmd"

and pExp () =
  match peek () with
  | TkIdent _ ->
      let _ = eat_ident () in
      eat TkLParen;
      pExpList ();
      eat TkRParen
  | TkVar _ -> 
      let _ = eat_variable () in ()
  | _ -> raise_parse_error "Exp"

and pExpList () =
  match peek () with
  | TkIdent _ 
  | TkVar _ -> 
      pExp ();
      pExpList' ()
  | TkRParen -> ()
  | _ -> raise_parse_error "ExpList"

and pExpList' () =
  match peek () with
  | TkRParen -> ()
  | TkComma -> 
      eat TkComma;
      pExpList ()
  | _ -> raise_parse_error "ExpList'"


(* Recogniser API *)

let recognise (s:string) : bool =
  tokens := lex s;
  try 
    pCmd ();
    true
  with
  | _ -> false
  