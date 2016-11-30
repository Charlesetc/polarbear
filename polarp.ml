
(*
 * Polarp is an abstract syntax tree that polarbear is first parsed into.
 * There are locations for each node and it should be pretty simple.
 * Operators are hardcoded/typed.
 *
 * This is not intended for use outside of the parsing world - it really
 * is an abstract syntax tree. polarp stands for polarbear-parsing-tree. polart
 * is polarbear-typed-tree, which is a tree that has types and is intended to
 * have more specific requirements than just parsing.
 *)

type location = Lexing.position * Lexing.position

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

type polarp
  = If of location
    (* condition *)
    * polarp
    (* true branch *)
    * polarp list
    (* optional false branch *)
    * polarp list option
  | For of location
    (* initialization *)
    * polarp
    (* condition *)
    * polarp
    (* step *)
    * polarp
    (* action *)
    * polarp
  | Apply of location
    (* function *)
    * polarp
    (* argument *)
    * polarp
  | Definition of location
    (* name *)
    * string
    (* item *)
    * polarp
  | Block of location
    (* arguments *)
    * string list
    (* list of items in block *)
    * polarp list
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
    * polarp
    (* second argument *)
    * polarp
  | UOperator of location
    (* which operator it is *)
    * operator_type
    (* argument *)
    * polarp
  | Field of location
    (* receiver *)
    * polarp
    (* name *)
    * string
  | Object of location
    (* list of fields to values *)
    * ( string * polarp ) list

let string_of_identifier (_, s) = s

let rec string_of_polarp polarp =
  match polarp with
  | If (_, condition, ifbranch, Some elsebranch) ->
    Printf.sprintf
      "(if %s [ %s ] else [ %s ])"
      (string_of_polarp condition)
      (String.concat " ; " (List.map string_of_polarp ifbranch))
      (String.concat " ; " (List.map string_of_polarp elsebranch))
  | If (_, condition, ifbranch, None) ->
    Printf.sprintf
      "(if %s [ %s ])"
      (string_of_polarp condition)
      (String.concat " ; " (List.map string_of_polarp ifbranch))
  | For (_, initialization, condition, step, action) ->
    Printf.sprintf
      "(for %s ; %s ; %s { %s })"
      (string_of_polarp initialization)
      (string_of_polarp condition)
      (string_of_polarp step)
      (string_of_polarp action)
  | Apply (_, f, argument) ->
    Printf.sprintf
      "(%s %s)"
      (string_of_polarp f)
      (string_of_polarp argument)
  | Definition (_, name, e) ->
    Printf.sprintf
      "(define %s %s)"
      name
      (string_of_polarp e)
  | Block (_, arguments, polarps) ->
      if (List.length arguments > 0) then
        ": " ^
        String.concat " " arguments ^
        " [ " ^
        String.concat " ; " (List.map string_of_polarp polarps) ^
        " ]"
      else
        "[ " ^
        String.concat " ; " (List.map string_of_polarp polarps) ^
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
      "(" ^ string_of_polarp a ^ " " ^ (string_of_operator_type operator_type) ^ " " ^ string_of_polarp b ^ ")"
  | UOperator (_, operator_type, e) ->
      "(" ^ (string_of_operator_type operator_type) ^ " " ^ string_of_polarp e ^ ")"
  | Field (_, receiver, name) ->
      "(" ^ string_of_polarp receiver ^ "." ^ name ^ ")"
  | Object (_, fields) ->
      "< " ^
      String.concat " ; " (List.map (fun (name, e) -> name ^ " = " ^ string_of_polarp e) fields) ^
      " >"
