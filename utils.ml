open Lexing

exception No_input of string
exception Exception of string

let char_for_backslash c = match c with
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let undefined () = raise (Exception "undefined")
