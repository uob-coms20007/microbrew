(** [token] is an enumeration of all possible tokens produced by the lexer *)
type token =
  | TkIdent of string
  | TkVar of string
  | TkLParen
  | TkRParen
  | TkDefine
  | TkComma
  | TkEquals
  | TkEnd

(* Recognising character classes *)

let is_lower (c:char) : bool =
  match c with
  | 'a' .. 'z' -> true
  | _          -> false

let is_upper (c:char) : bool = 
  match c with
  | 'A' .. 'Z' -> true
  | _          -> false

let is_wspace (c:char) : bool =
  match c with
  | ' ' | '\n' | '\r' -> true
  | _                 -> false

let is_letter (c:char) : bool =
  is_lower c || is_upper c


(* Input string state *)

let idx = ref 0
let input = ref ""

(* Output token list state *)
let output = ref []

(* Simple API for input and output state *)

let peek () : char option =
  if !idx < String.length !input then 
    Some (!input.[!idx])
  else
    None

let drop () =
  idx := !idx + 1

let emit (tk:token) =
  output := !output @ [tk]

let raise_lex_error exp =
  let msg = Printf.sprintf "LEX ERROR: Expected %s but found %c." exp (!input.[!idx])
  in failwith msg

(* 
  Lexing functions (lexer states) 

  There are two lexer states:
    * init
    * var_or_id_or_kw
*)

(** 
  The var_or_id_or_kw state has a string parameter [lexeme].

  If the lexer is currently executing the function call:
    [lex_var_or_id_or_kw "foo"]
  this means that the lexer is in state var_or_id_or_kw and
  the lexeme that it has recognised so far is ["foo"] (i.e. the 
  previous three characters of the string were ['o'], ['o'] and ['f'], and 
  before that there was either no character (the start of the string)
  or a non-letter character.
*)
let rec lex_var_or_id_or_kw (lexeme: string) =
  match peek () with
  | Some c when is_letter c -> 
      drop ();
      (* Return to this same state *)
      lex_var_or_id_or_kw (lexeme ^ String.make 1 c)
  | _ ->
      (* We have reached the end of the lexeme, 
        Check if it is the keyword "def", 
        otherwise it's a variable or identifier. *)
      (match lexeme with
      | "def" -> emit TkDefine
      | _     -> 
          (* Determine if it is a variable or identifier *)
          if is_lower (lexeme.[0]) then 
            emit (TkVar lexeme) 
          else 
            emit (TkIdent lexeme)
      );
      (* Continue in the initial lexer state *)
      lex_init ()
  
(* 
  The init state takes no parameter (because there is no need
  to remember any substring when in this state).
*)
and lex_init () =
  match peek () with
  | None -> emit TkEnd
  | Some '(' ->
      drop ();
      emit TkLParen;
      lex_init ()
  | Some ')' ->
      drop ();
      emit TkRParen;
      lex_init ()
  | Some '=' ->
      drop ();
      emit TkEquals;
      lex_init ()
  | Some ',' ->
      drop ();
      emit TkComma;
      lex_init ()
  | Some c when is_wspace c -> 
      drop ();
      lex_init ()
  | Some c when is_letter c -> 
      (* Move to lexer state var_or_id_or_kw with a so far empty lexeme *)
      lex_var_or_id_or_kw ("")
  | _ -> raise_lex_error "valid character"

(* API of the lexer *)

(** 
    [lex s] returns the token list obtained by scanning [s].
    @raises [Failure] if [s] fails to scan.
*)
let lex (s:string) : token list =
  (* Setup the internal variables *)
  input := s;
  idx := 0;
  output := [];
  (* Begin lexing in the initial state *)
  lex_init ();
  (* Return the output *)
  !output

(* Conversion of token to a string, for debugging purposes. *)

(** [string_of_token tk] returns the string representation of [tk]. 
    Note: this is not necessarily the lexeme from which [tk] was obtained. *)
let string_of_token tk =
  match tk with
  | TkIdent s  -> s
  | TkVar s    -> s
  | TkLParen   -> "("
  | TkRParen   -> ")"
  | TkDefine   -> "def"
  | TkEquals   -> "="
  | TkComma    -> ","
  | TkEnd      -> "$"

(* Inline Testing *)

let%test _ =
  let expected = [
    TkIdent "Z";
    TkLParen;
    TkRParen;
    TkEnd
  ]
  in lex "Z()" = expected

let%test _ =
  let expected = [
    TkDefine;
    TkIdent "One";
    TkLParen;
    TkRParen;
    TkEquals;
    TkIdent "S";
    TkLParen;
    TkIdent "Z";
    TkLParen;
    TkRParen;
    TkRParen;
    TkEnd
  ]
  in lex "def One() = S(Z())" = expected

let%test _ =
  let expected = [
    TkDefine;
    TkIdent "Head";
    TkLParen;
    TkIdent "C";
    TkLParen;
    TkVar "x";
    TkComma;
    TkVar "xs";
    TkRParen;
    TkRParen;
    TkEquals;
    TkVar "x";
    TkEnd
  ]
  in lex "def Head(C(x,xs)) = x" = expected

let%test _ = 
  let expected = [
    TkIdent "Add";
    TkLParen;
    TkIdent "S";
    TkLParen;
    TkIdent "Z";
    TkLParen;
    TkRParen;
    TkRParen;
    TkComma;
    TkIdent "S";
    TkLParen;
    TkIdent "Z";
    TkLParen;
    TkRParen;
    TkRParen;
    TkRParen;
    TkEnd
  ]
  in lex "Add(S(Z()), S(Z()))" = expected
