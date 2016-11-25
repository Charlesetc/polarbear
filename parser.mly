
(* TOKENS *)

(* Identifiers *)

%token <string> IDENT

(* Primitives *)

%token <int> INT
%token <string> FLOAT
%token <string> STRING

(* Delimiters *)
%token EOL
%token EOF
%token SPACE

(* Matched delimiters *)
(* %token OPEN_ROUND *)
(* %token CLOSE_ROUND *)
(* %token OPEN_SQUARE *)
(* %token CLOSE_SQUARE *)

(* Keywords *)
(* %token IF *)
(* %token DEFINE *)
(* %token ELSE *)

(* START *)

%start <Polart.polart list> polart

%%

(* NON TERMINALS *)

polart:
  | a = items end_of_file { a }

end_of_file:
  | lines EOF { () }

lines:
  | lines EOL { () }
  | EOL { () }

items:
  | bc = items EOL a = expr { a :: bc }
  | a = expr { [a] }

expr:
  | a = atom { a }
  | a = fapp { a }

fapp:
  | a = atom_with_fapp SPACE b = atom { Polart.Apply (Polart.null_location, a, b) }

atom_with_fapp:
  | a = atom { a }
  | a = fapp { a }

atom:
  | i = INT
    { Polart.Int (Polart.null_location, i) }
  | s = STRING
    { Polart.String (Polart.null_location, s) }
  | ident = IDENT
    { Polart.Variable (Polart.null_location, ident) }
  | f = FLOAT
    { Polart.Float (Polart.null_location, f) }
