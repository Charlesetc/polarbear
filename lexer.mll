{
  open Parser

  exception Error of string
}

rule token = parse
| [' ' '\t']
  { SPACE }
| ['\n' ';']
  { EOL }
| ['0'-'9']* '.' ['0'-'9']+ as f
  { FLOAT f }
| ['0'-'9']+ '.' ['0'-'9']* as f
  { FLOAT f }
| ['0'-'9']+ as i
  { INT (int_of_string i) }
| '('
  { OPEN_ROUND }
| ')'
  { CLOSE_ROUND }
| ['a'-'z']+ as ident
  { IDENT ident }
| '"' (_* as string) '"'
  { STRING string }
| '#' [^ '\n'] *
  { EOL }
| _
  { raise (Error (Printf.sprintf "%d: unexpected character %s.\n" (Lexing.lexeme_start lexbuf) (Lexing.lexeme lexbuf))) }
| eof
  { EOF }

