
(*
 *
 *)

type location = Lexing.position * Lexing.position

type identifier = location * string

type polartype = Generic of string
               | Int
               | Float
               | String

let string_of_polartype x = match x with
  | Generic s -> s
  | Int -> "Int"
  | Float -> "Float"
  | String -> "String"


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
  = If of location * polartype
    (* condition *)
    * polart
    (* true branch *)
    * polart list
    (* optional false branch *)
    * polart list option
  | For of location * polartype
    (* initialization *)
    * polart
    (* condition *)
    * polart
    (* step *)
    * polart
    (* action *)
    * polart
  | Apply of location * polartype
    (* function *)
    * polart
    (* argument *)
    * polart
  | Definition of location * polartype
    (* name *)
    * string
    (* item *)
    * polart
  | Block of location * polartype
    (* arguments *)
    * string list
    (* list of items in block *)
    * polart list
  | Int of location * polartype * int
  | Float of location * polartype
    (* string because it would be terrible
     * to get a precision error in the compiler.
     * *)
    * string
  | String of location * polartype * string
  | Unit of location * polartype
  | Variable of location * polartype * string
  | Field of location * polartype
    (* receiver *)
    * polart
    (* name *)
    * string
  | Object of location * polartype
    (* list of fields to values *)
    * ( string * polart ) list

let string_of_identifier (_, s) = s

let rec string_of_polart polart =
  match polart with
  | If (_, polartype, condition, ifbranch, Some elsebranch) ->
    Printf.sprintf
      "( %s | if %s [ %s ] else [ %s ])"
      (string_of_polartype polartype)
      (string_of_polart condition)
      (String.concat " ; " (List.map string_of_polart ifbranch))
      (String.concat " ; " (List.map string_of_polart elsebranch))
  | If (_, polartype, condition, ifbranch, None) ->
    Printf.sprintf
      "( %s | if %s [ %s ])"
      (string_of_polartype polartype)
      (string_of_polart condition)
      (String.concat " ; " (List.map string_of_polart ifbranch))
  | For (_, polartype, initialization, condition, step, action) ->
    Printf.sprintf
      "(%s | for %s ; %s ; %s { %s })"
      (string_of_polartype polartype)
      (string_of_polart initialization)
      (string_of_polart condition)
      (string_of_polart step)
      (string_of_polart action)
  | Apply (_, polartype, f, argument) ->
    Printf.sprintf
      "( %s | %s %s)"
      (string_of_polartype polartype)
      (string_of_polart f)
      (string_of_polart argument)
  | Definition (_, polartype, name, e) ->
    Printf.sprintf
      "( %s | define %s %s)"
      (string_of_polartype polartype)
      name
      (string_of_polart e)
  | Block (_, polartype, arguments, polarts) ->
      if (List.length arguments > 0) then
        ": " ^
        String.concat " " arguments ^
        " | " ^
        (string_of_polartype polartype) ^
        " [ " ^
        String.concat " ; " (List.map string_of_polart polarts) ^
        " ]"
      else
        "[ " ^
        String.concat " ; " (List.map string_of_polart polarts) ^
        " ]"
  | Int (_, polartype, i) ->
      Printf.sprintf "%s | %d"
        (string_of_polartype polartype)
        i
  | Float (_, polartype, f) ->
      Printf.sprintf "%s | %s"
        (string_of_polartype polartype)
        f
  | String (_, polartype, s) ->
      Printf.sprintf "%s | \"%s\""
        (string_of_polartype polartype)
        s
  | Unit (_, polartype) ->
      Printf.sprintf "%s | ()"
        (string_of_polartype polartype)
  | Variable (_, polartype, name) ->
      Printf.sprintf "%s | %s"
        (string_of_polartype polartype)
        name
  | Field (_, polartype, receiver, name) ->
      "( " ^ (string_of_polartype polartype) ^ " | " ^ string_of_polart receiver ^ "." ^ name ^ ")"
  | Object (_, polartype, fields) ->
      "| " ^ string_of_polartype polartype ^ " < " ^
      String.concat " ; " (List.map (fun (name, e) -> name ^ " = " ^ string_of_polart e) fields) ^
      " >"
