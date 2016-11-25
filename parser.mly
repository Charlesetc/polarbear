
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
%token OPEN_ROUND
%token CLOSE_ROUND
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
  | bc = items lines a = expr { a :: bc }
  | a = expr { [a] }

expr:
  | a = atom { a }
  | a = fapp { a }

multiline_expr:
  | a = atom { a }
  | a = multiline_fapp { a }

(* optional_spaces: *)
(*   { () } *)
(*   | spaces *)
(*   { () } *)

spaces:
  | spaces SPACE
    { () }
  | SPACE
    { () }

multiline_spaces:
  | spaces SPACE
    { () }
  | spaces EOL
    { () }
  | SPACE
    { () }
  | EOL
    { () }

fapp:
  | a = atom_with_fapp spaces b = atom { Polart.Apply (Polart.null_location, a, b) }

multiline_fapp:
  | a = atom_with_multiline_fapp multiline_spaces b = atom { Polart.Apply (Polart.null_location, a, b) }

atom_with_multiline_fapp:
  | a = atom { a }
  | a = multiline_fapp { a }

atom_with_fapp:
  | a = atom { a }
  | a = fapp { a }

atom:
  | OPEN_ROUND a = multiline_expr CLOSE_ROUND
    { a }
  | i = INT
    { Polart.Int (Polart.null_location, i) }
  | s = STRING
    { Polart.String (Polart.null_location, s) }
  | ident = IDENT
    { Polart.Variable (Polart.null_location, ident) }
  | f = FLOAT
    { Polart.Float (Polart.null_location, f) }
