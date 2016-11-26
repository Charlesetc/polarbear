{
  open Parser
  open Utils

  exception Error of string
}


rule token = parse
(* Keywords *)
(* | "if" *)
(*   { IF } *)
(* | "else" *)
(*   { IF } *)
(* | "define" *)
(*   { DEFINE } *)

(* Delimiters *)
| [' ' '\t']
  { token lexbuf }
| ['\n' ';']
  { EOL }
| '#' [^ '\n'] *
  { EOL }
| eof
  { EOF }

(* Operators *)
| '-'
  { MINUS }
| '+'
  { PLUS }
| '*'
  { TIMES }
| '/'
  { DIVIDE }
| ':'
  { COLON }

(* Primitives *)
| ['0'-'9']* '.' ['0'-'9']+ as f
  { FLOAT f }
| ['0'-'9']+ '.' ['0'-'9']* as f
  { FLOAT f }
| ['0'-'9']+ as i
  { INT (int_of_string i) }
| '"'
  { STRING (token_string lexbuf) }

(* Matched delimiters *)
| '('
  { OPEN_ROUND }
| ')'
  { CLOSE_ROUND }
| '['
  { OPEN_SQUARE }
| ']'
  { CLOSE_SQUARE }

(* Everything else *)
| ['a'-'z']+ as ident
  { IDENT ident }
| _
  { raise (Error (Printf.sprintf "%d: unexpected character %s.\n" (Lexing.lexeme_start lexbuf) (Lexing.lexeme lexbuf))) }

and token_string = parse
  '"'
      { "" }
  | '\\' (['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] as c)
    { String.make 1 (char_for_backslash c) ^ token_string lexbuf }
  | '\\' '\n' ([' ' '\t'] * as space)
     { space ^ token_string lexbuf }
  | '\\' _
      { raise (Error "illegal escaped character") }
  | eof
      { raise (Error "unterminated string") }
  | _ as c
     { String.make 1 c ^ token_string lexbuf }
