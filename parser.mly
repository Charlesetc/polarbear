
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

(* Operators *)

%token PLUS
%token MINUS
%token TIMES
%token DIVIDE


(* Matched delimiters *)
%token OPEN_ROUND
%token CLOSE_ROUND
(* %token OPEN_SQUARE *)
(* %token CLOSE_SQUARE *)

(* Keywords *)
(* %token IF *)
(* %token DEFINE *)
(* %token ELSE *)

(* PRECEDENCE *)

%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS

(* START *)

%start <Polart.polart list> polart

%%

(* NON TERMINALS *)

polart:
  | multiline_spaces a = items end_of_file { a }

end_of_file:
  | lines EOF { () }

lines:
  | EOL lines { () }
  | EOL { () }

items:
  | bc = items lines a = expr { a :: bc }
  | a = expr { [a] }

expr:
  | a = atom { a }
  | a = fapp { a }
  | a = expr PLUS b = expr { Polart.Operator (Polart.null_location, Polart.PLUS, a, b) }
  | a = expr TIMES b = expr { Polart.Operator (Polart.null_location, Polart.TIMES, a, b) }
  | a = expr DIVIDE b = expr { Polart.Operator (Polart.null_location, Polart.DIVIDE, a, b) }
  | a = expr MINUS b = expr { Polart.Operator (Polart.null_location, Polart.MINUS, a, b) }
  | MINUS e = expr %prec UMINUS { Polart.UOperator (Polart.null_location, Polart.MINUS, e) }

multiline_expr:
  | a = atom { a }
  | a = multiline_fapp { a }
  | a = multiline_expr PLUS b = multiline_expr { Polart.Operator (Polart.null_location, Polart.PLUS, a, b) }
  | a = multiline_expr TIMES b = multiline_expr { Polart.Operator (Polart.null_location, Polart.TIMES, a, b) }
  | a = multiline_expr DIVIDE b = multiline_expr { Polart.Operator (Polart.null_location, Polart.DIVIDE, a, b) }
  | a = multiline_expr MINUS b = multiline_expr { Polart.Operator (Polart.null_location, Polart.MINUS, a, b) }
  | MINUS e = expr %prec UMINUS { Polart.UOperator (Polart.null_location, Polart.MINUS, e) }

(* optional_spaces: *)
(*   { () } *)
(*   | spaces *)
(*   { () } *)

multiline_spaces:
  { () }
  | EOL multiline_spaces
    { () }

fapp:
  | a = atom_with_fapp b = atom { Polart.Apply (Polart.null_location, a, b) }

multiline_fapp:
  | a = atom_with_multiline_fapp multiline_spaces b = atom { Polart.Apply (Polart.null_location, a, b) }

atom_with_multiline_fapp:
  | a = atom { a }
  | a = multiline_fapp { a }

atom_with_fapp:
  | a = atom { a }
  | a = fapp { a }

atom:
  | OPEN_ROUND multiline_spaces a = multiline_expr CLOSE_ROUND
    { a }
  | i = INT
    { Polart.Int (Polart.null_location, i) }
  | s = STRING
    { Polart.String (Polart.null_location, s) }
  | ident = IDENT
    { Polart.Variable (Polart.null_location, ident) }
  | f = FLOAT
    { Polart.Float (Polart.null_location, f) }
