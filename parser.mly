
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
%token EQUALS

%token COLON
%token <string> FIELD


(* Matched delimiters *)
%token OPEN_ROUND
%token CLOSE_ROUND
%token OPEN_SQUARE
%token CLOSE_SQUARE

(* Keywords *)
%token IF
%token ELSE
%token DEFINE
%token LET

(* PRECEDENCE *)

%nonassoc LET
(* %nonassoc IF *)
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS DEFINE

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
  (* most basic expression *)
  | a = atom { a }

  (* function application *)
  | a = fapp { a }

  (* operators *)
  | a = expr PLUS multiline_spaces b = expr { Polart.Operator (Polart.null_location, Polart.PLUS, a, b) }
  | a = expr TIMES multiline_spaces b = expr { Polart.Operator (Polart.null_location, Polart.TIMES, a, b) }
  | a = expr DIVIDE multiline_spaces b = expr { Polart.Operator (Polart.null_location, Polart.DIVIDE, a, b) }
  | a = expr MINUS multiline_spaces b = expr { Polart.Operator (Polart.null_location, Polart.MINUS, a, b) }
  | MINUS e = expr %prec UMINUS { Polart.UOperator (Polart.null_location, Polart.MINUS, e) }

  (* definitions *)
  | DEFINE i = IDENT e = expr %prec DEFINE { Polart.Definition (Polart.null_location, i, e) }
  | LET i = IDENT EQUALS multiline_spaces e = expr %prec LET { Polart.Definition (Polart.null_location, i, e) }

  (* conditionals *)
  | i = if_condition { i }


if_condition:
  | IF condition = atom multiline_spaces OPEN_SQUARE multiline_spaces body = items multiline_spaces CLOSE_SQUARE { Polart.If (Polart.null_location, condition, body, None) }
  | IF condition = atom multiline_spaces OPEN_SQUARE multiline_spaces body = items multiline_spaces CLOSE_SQUARE ELSE multiline_spaces OPEN_SQUARE multiline_spaces elsebody = items multiline_spaces CLOSE_SQUARE { Polart.If (Polart.null_location, condition, body, Some elsebody) }
  | IF condition = atom multiline_spaces OPEN_SQUARE multiline_spaces body = items multiline_spaces CLOSE_SQUARE ELSE multiline_spaces elsebody = if_condition { Polart.If (Polart.null_location, condition, body, Some [elsebody]) }

multiline_expr:
  (* most basic expression *)
  | a = atom { a }

  (* function application *)
  | a = multiline_fapp { a }

  (* operators *)
  | a = multiline_expr PLUS b = multiline_expr { Polart.Operator (Polart.null_location, Polart.PLUS, a, b) }
  | a = multiline_expr TIMES b = multiline_expr { Polart.Operator (Polart.null_location, Polart.TIMES, a, b) }
  | a = multiline_expr DIVIDE b = multiline_expr { Polart.Operator (Polart.null_location, Polart.DIVIDE, a, b) }
  | a = multiline_expr MINUS b = multiline_expr { Polart.Operator (Polart.null_location, Polart.MINUS, a, b) }
  | MINUS e = expr %prec UMINUS { Polart.UOperator (Polart.null_location, Polart.MINUS, e) }

  (* definitions *)
  | DEFINE multiline_spaces i = IDENT multiline_spaces e = expr %prec DEFINE { Polart.Definition (Polart.null_location, i, e) }
  | LET multiline_spaces i = IDENT multiline_spaces EQUALS multiline_spaces e = expr %prec LET { Polart.Definition (Polart.null_location, i, e) }

  (* conditionals *)
  | i = multiline_if_condition { i }


multiline_if_condition:
  | IF multiline_spaces condition = atom multiline_spaces OPEN_SQUARE multiline_spaces body = items multiline_spaces CLOSE_SQUARE { Polart.If (Polart.null_location, condition, List.rev body, None) }
  | IF multiline_spaces condition = atom multiline_spaces OPEN_SQUARE multiline_spaces body = items multiline_spaces CLOSE_SQUARE multiline_spaces ELSE multiline_spaces OPEN_SQUARE multiline_spaces elsebody = items multiline_spaces CLOSE_SQUARE { Polart.If (Polart.null_location, condition, List.rev body, Some (List.rev elsebody)) }
  | IF multiline_spaces condition = atom multiline_spaces OPEN_SQUARE  multiline_spaces body = items multiline_spaces CLOSE_SQUARE multiline_spaces ELSE multiline_spaces elsebody = multiline_if_condition { Polart.If (Polart.null_location, condition, List.rev body, Some [elsebody]) }


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

list_of_ident:
  | a = IDENT b = list_of_ident
    { a :: b }
  | a = IDENT
    { [a] }

atom:
  (* parentheses *)
  | OPEN_ROUND multiline_spaces a = multiline_expr CLOSE_ROUND
    { a }

  (* dot syntax *)
  | receiver = atom name = FIELD { Polart.Field (Polart.null_location, receiver, name) }

  (* unit *)
  | OPEN_ROUND multiline_spaces CLOSE_ROUND
    { Polart.Unit Polart.null_location }

  (* lambdas *)
  | OPEN_SQUARE multiline_spaces a = items multiline_spaces CLOSE_SQUARE
    { Polart.Block (Polart.null_location, [], List.rev a) }
  | COLON multiline_spaces a = list_of_ident multiline_spaces OPEN_SQUARE multiline_spaces b = items multiline_spaces CLOSE_SQUARE
    { Polart.Block (Polart.null_location, a, List.rev b) }

  (* simple tokens *)
  | i = INT
    { Polart.Int (Polart.null_location, i) }
  | s = STRING
    { Polart.String (Polart.null_location, s) }
  | ident = IDENT
    { Polart.Variable (Polart.null_location, ident) }
  | f = FLOAT
    { Polart.Float (Polart.null_location, f) }
