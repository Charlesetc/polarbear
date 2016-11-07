%token <int> INT
%token <string> FLOAT
%token <string> STRING
%token <string> IDENT
%token OPEN_ROUND
%token CLOSE_ROUND
%token LINE_COMMENT
%token EOL
%token EOF
%token SPACE

%start <Hzlt.hzlt list> hzlt

%%

hzlt:
  | a = expr b = hzlt
    { a :: b } 
  | end_of_line a = hzlt
    { a }
  | space end_of_line a = hzlt
    { a }
  | EOF
    { [] }

space:
  | SPACE space
    { () }
  | SPACE
    { () }

end_of_line:
  | end_of_line space
    { () }
  | EOL
    { () }

multiline_space:
  | multiline_space multiline_space
    { () }
  | SPACE
    { () }
  | EOL
    { () }

expr:
  | a = expr space b = expr
    { Hzlt.Apply (Hzlt.null_location, a, b) }
  | a = atom
    { a }
  | OPEN_ROUND e = multiline_expr CLOSE_ROUND
    { e }

multiline_expr:
  | a = multiline_expr multiline_space b = multiline_expr
    { Hzlt.Apply (Hzlt.null_location, a, b) }
  | a = atom
    { a }

  | multiline_space a = multiline_expr
    { a }
  | a = multiline_expr multiline_space
    { a }
  | OPEN_ROUND e = multiline_expr CLOSE_ROUND
    { e }

atom:
  | i = INT
    { Hzlt.Int (Hzlt.null_location, i) }
  | s = STRING
    { Hzlt.String (Hzlt.null_location, s) }
  | ident = IDENT
    { Hzlt.Variable (Hzlt.null_location, ident) }
  | f = FLOAT
    { Hzlt.Float (Hzlt.null_location, f) }
