
(*
 * This file defines a tree Snowt
 * which is intended to be used for
 * a lower level, more semantically
 * aware tree.
 *
 * Snowt is very similar to the
 * parsing tree Snowp, but it also
 * has types and no operators.
 * *)

(* open Snowp *)

type location = Lexing.position * Lexing.position * string (* filename *)

type snowt
  = If of location
    (* condition *)
    * snowt
    (* true branch *)
    * snowt list
    (* optional false branch *)
    * snowt list option
  | For of location
    (* initialization *)
    * snowt
    (* condition *)
    * snowt
    (* step *)
    * snowt
    (* action *)
    * snowt list
  | Apply of location
    (* function *)
    * snowt
    (* argument *)
    * snowt
  | Definition of location
    (* name *)
    * string
    (* item *)
    * snowt
  | Block of location
    (* arguments *)
    * string list
    (* list of items in block *)
    * snowt list
  | Int of location * int
  | Float of location
    (* string because it would be terrible
     * to get a precision error in the compiler.
     * *)
    * string
  | String of location * string
  | Unit of location
  | Variable of location * string
  | Field of location
    (* receiver *)
    * snowt
    (* name *)
    * string
  | Object of location
    (* list of fields to values *)
    * ( string * snowt ) list

let string_of_identifier (_, s) = s

let rec string_of_snowt snowt =
  match snowt with
  | If (_, condition, ifbranch, Some elsebranch) ->
    Printf.sprintf
      "( if %s [ %s ] else [ %s ])"
      (string_of_snowt condition)
      (String.concat " ; " (List.map string_of_snowt ifbranch))
      (String.concat " ; " (List.map string_of_snowt elsebranch))
  | If (_, condition, ifbranch, None) ->
    Printf.sprintf
      "( if %s [ %s ])"
      (string_of_snowt condition)
      (String.concat " ; " (List.map string_of_snowt ifbranch))
  | For (_, initialization, condition, step, actions) ->
    Printf.sprintf
      "(for %s ; %s ; %s { %s })"
      (string_of_snowt initialization)
      (string_of_snowt condition)
      (string_of_snowt step)
      (String.concat " ; " (List.map string_of_snowt actions))
  | Apply (_, f, argument) ->
    Printf.sprintf
      "(%s %s)"
      (string_of_snowt f)
      (string_of_snowt argument)
  | Definition (_, name, e) ->
    Printf.sprintf
      "(define %s %s)"
      name
      (string_of_snowt e)
  | Block (_, arguments, snowts) ->
      if (List.length arguments > 0) then
        ": " ^
        String.concat " " arguments ^
        " [ " ^
        String.concat " ; " (List.map string_of_snowt snowts) ^
        " ]"
      else
        "[ " ^
        String.concat " ; " (List.map string_of_snowt snowts) ^
        " ]"
  | Int (_, i) ->
      Printf.sprintf "%d"
        i
  | Float (_, f) ->
      Printf.sprintf "%s"
        f
  | String (_, s) ->
      Printf.sprintf "\"%s\""
        s
  | Unit (_) ->
      Printf.sprintf "()"
  | Variable (_, name) ->
      Printf.sprintf "%s"
        name
  | Field (_, receiver, name) ->
      "( " ^ string_of_snowt receiver ^ "." ^ name ^ ")"
  | Object (_, fields) ->
      " < " ^
      String.concat " ; " (List.map (fun (name, e) -> name ^ " = " ^ string_of_snowt e) fields) ^
      " >"
