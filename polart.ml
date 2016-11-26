
type location = {
  column : int ;
  row : int ;
}

let null_location = { column = 0 ; row = 0 }

type identifier = location * string

type operator_type =
  | PLUS
  | MINUS
  | TIMES
  | DIVIDE

let string_of_operator_type = function
  | PLUS -> "+"
  | TIMES -> "*"
  | MINUS -> "-"
  | DIVIDE -> "/"

type polart
  = Let of location
    (* identifier assigned to *)
    * identifier
    (* expression *)
    * polart
  | If of location
    (* condition *)
    * polart
    (* true branch *)
    * polart
    (* optional false branch *)
    * polart option
  | For of location
    (* initialization *)
    * polart
    (* condition *)
    * polart
    (* step *)
    * polart
    (* action *)
    * polart
  | Apply of location
    (* function *)
    * polart
    (* argument *)
    * polart
  | Definition of location
    (* name of the function *)
    * identifier
    (* argument list *)
    * identifier list
    (* body *)
    * polart
  | Block of location
    (* arguments *)
    * string list
    (* list of items in block *)
    * polart list
  | Int of location * int
  | Float of location
    (* string because it would be terrible
     * to get a precision error in the compiler.
     * *)
    * string
  | String of location * string
  | Unit of location
  | Variable of location * string
  | Operator of location
    (* which operator it is *)
    * operator_type
    (* first argument *)
    * polart
    (* second argument *)
    * polart
  | UOperator of location
    (* which operator it is *)
    * operator_type
    (* argument *)
    * polart

let string_of_identifier (_, s) = s

let rec string_of_polart polart =
  match polart with
  | Let (_, identifier, child) ->
      Printf.sprintf
        "(let %s = %s)"
        (string_of_identifier identifier)
        (string_of_polart child)
  | If (_, condition, ifbranch, Some elsebranch) ->
    Printf.sprintf
      "(if %s { %s } else { %s })"
      (string_of_polart condition)
      (string_of_polart ifbranch)
      (string_of_polart elsebranch)
  | If (_, condition, ifbranch, None) ->
    Printf.sprintf
      "(if %s { %s })"
      (string_of_polart condition)
      (string_of_polart ifbranch)
  | For (_, initialization, condition, step, action) ->
    Printf.sprintf
      "(for %s ; %s ; %s { %s })"
      (string_of_polart initialization)
      (string_of_polart condition)
      (string_of_polart step)
      (string_of_polart action)
  | Apply (_, f, argument) ->
    Printf.sprintf
      "(%s %s)"
      (string_of_polart f)
      (string_of_polart argument)
  | Definition (_, function_name, arguments, body) ->
    Printf.sprintf
      "(define %s %s { %s })"
      (string_of_identifier function_name)
      (String.concat " "
        (List.map string_of_identifier arguments))
      (string_of_polart body)
  | Block (_, arguments, polarts) ->
      ": " ^
      String.concat " " arguments ^
      " [ " ^
      String.concat " ; " (List.map string_of_polart polarts) ^
      " ]"
  | Int (_, i) ->
      Printf.sprintf "%d" i
  | Float (_, f) ->
      Printf.sprintf "%s" f
  | String (_, s) ->
      Printf.sprintf "\"%s\"" s
  | Unit _ ->
      "()"
  | Variable (_, name) ->
      Printf.sprintf "%s" name
  | Operator (_, operator_type, a, b) ->
      "(" ^ string_of_polart a ^ " " ^ (string_of_operator_type operator_type) ^ " " ^ string_of_polart b ^ ")"
  | UOperator (_, operator_type, e) ->
      "(" ^ (string_of_operator_type operator_type) ^ " " ^ string_of_polart e ^ ")"
