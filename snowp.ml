
(*
 * Snowp is an abstract syntax tree that snow is first parsed into.
 * There are locations for each node and it should be pretty simple.
 * Operators are hardcoded/typed.
 *
 * This is not intended for use outside of the parsing world - it really
 * is an abstract syntax tree. snowp stands for snow-parsing-tree.
 * *)

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

let string_of_unary_operator_type = function
  | MINUS -> "-"
  (* silent erroring is easier to develop with here *)
  | _ -> "(are you sure this is a unary operator?)"

type snowp
  = If of location
    (* condition *)
    * snowp
    (* true branch *)
    * snowp list
    (* optional false branch *)
    * snowp list option
  | For of location
    (* initialization *)
    * snowp
    (* condition *)
    * snowp
    (* step *)
    * snowp
    (* actions *)
    * snowp list
  | Apply of location
    (* function *)
    * snowp
    (* argument *)
    * snowp
  | Definition of location
    (* name *)
    * string
    (* item *)
    * snowp
  | Block of location
    (* arguments *)
    * string list
    (* list of items in block *)
    * snowp list
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
    * snowp
    (* second argument *)
    * snowp
  | UOperator of location
    (* which operator it is *)
    * operator_type
    (* argument *)
    * snowp
  | Field of location
    (* receiver *)
    * snowp
    (* name *)
    * string
  | Object of location
    (* list of fields to values *)
    * ( string * snowp ) list

let string_of_identifier (_, s) = s

let rec string_of_snowp snowp =
  match snowp with
  | If (_, condition, ifbranch, Some elsebranch) ->
    Printf.sprintf
      "(if %s [ %s ] else [ %s ])"
      (string_of_snowp condition)
      (String.concat " ; " (List.map string_of_snowp ifbranch))
      (String.concat " ; " (List.map string_of_snowp elsebranch))
  | If (_, condition, ifbranch, None) ->
    Printf.sprintf
      "(if %s [ %s ])"
      (string_of_snowp condition)
      (String.concat " ; " (List.map string_of_snowp ifbranch))
  | For (_, initialization, condition, step, actions) ->
    Printf.sprintf
      "(for %s ; %s ; %s { %s })"
      (string_of_snowp initialization)
      (string_of_snowp condition)
      (string_of_snowp step)
      (String.concat " ; " (List.map string_of_snowp actions))
  | Apply (_, f, argument) ->
    Printf.sprintf
      "(%s %s)"
      (string_of_snowp f)
      (string_of_snowp argument)
  | Definition (_, name, e) ->
    Printf.sprintf
      "(define %s %s)"
      name
      (string_of_snowp e)
  | Block (_, arguments, snowps) ->
      if (List.length arguments > 0) then
        ": " ^
        String.concat " " arguments ^
        " [ " ^
        String.concat " ; " (List.map string_of_snowp snowps) ^
        " ]"
      else
        "[ " ^
        String.concat " ; " (List.map string_of_snowp snowps) ^
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
      "(" ^ string_of_snowp a ^ " " ^ (string_of_operator_type operator_type) ^ " " ^ string_of_snowp b ^ ")"
  | UOperator (_, operator_type, e) ->
      "(" ^ (string_of_operator_type operator_type) ^ " " ^ string_of_snowp e ^ ")"
  | Field (_, receiver, name) ->
      "(" ^ string_of_snowp receiver ^ "." ^ name ^ ")"
  | Object (_, fields) ->
      "< " ^
      String.concat " ; " (List.map (fun (name, e) -> name ^ " = " ^ string_of_snowp e) fields) ^
      " >"

let function_of_operator location operator_type a b : snowp =
  Apply (location, Field (location, a, string_of_operator_type operator_type), b)

let function_of_uoperator location operator_type a : snowp =
  Apply (location, Field (location, a, string_of_unary_operator_type operator_type), Unit location)
