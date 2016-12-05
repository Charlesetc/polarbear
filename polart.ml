
(*
 * This file defines a type Polart
 * which is intended to be used for
 * type inference and type checking.
 *
 * Polart is very similar to the
 * parsing tree Polarp, but it also
 * has types and no operators.
 * *)

open Polarp

type polartype = Generic of string
               | Integer
               | Float
               | String
               | Function of
                   (* function *)
                   polartype
                   (* argument *)
                   * polartype

let rec string_of_polartype x = match x with
  | Generic s -> s
  | Integer -> "int"
  | Float -> "float"
  | String -> "string"
  | Function (t1, t2) ->
      "(" ^
      string_of_polartype t1 ^
      " -> " ^
      string_of_polartype t2 ^
      ")"

(* this is a nice way to make short generic names *)
let generic_type () =
  let index = ref 0 in
  let base = Char.code 'a' in
  let rec string_of_index = function
    | 0 -> ""
    | i ->
      let chr = i mod 26 in
      base + chr
        |> Char.chr
        |> Char.escaped
        |> (^) (string_of_index (i / 26)) in
  function () ->
    index := !index + 1 ; 
    Generic (string_of_index !index)

let initial_generator = generic_type ()
let initial_type () = initial_generator ()

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
    * polart list
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
    * (string * polartype) list
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

(* mapate over a polart *)
let rec polarts_iter f = List.map (polart_iter f)
and object_polart_iter f xs = match xs with
  | [] -> []
  | (x , b) :: rest -> (x , polart_iter f b) :: object_polart_iter f rest
and polart_iter f polart =
  let polart_iter = polart_iter f in
  let polarts_iter = polarts_iter f in
  let object_polart_iter = object_polart_iter f in

  let callback : unit -> unit = f polart in
  let return_value = match polart with
    | If (location, polartype, condition, ifbranch, Some elsebranch) ->
      If (location, polartype, polart_iter condition, polarts_iter ifbranch, Some (polarts_iter elsebranch))
    | If (location, polartype, condition, ifbranch, None) ->
      If (location, polartype, polart_iter condition, polarts_iter ifbranch, None)
    | For (location, polartype, initialization, condition, step, actions) ->
      For (location, polartype, polart_iter initialization, polart_iter condition, polart_iter step, polarts_iter actions)
    | Apply (location, polartype, f, argument) ->
      Apply (location, polartype, polart_iter f, polart_iter argument)
    | Definition (location, polartype, name, e) ->
      Definition (location, polartype, name, polart_iter e)
    | Block (location, polartype, arguments, polarts) ->
      Block (location, polartype, arguments, polarts_iter polarts)
    | Int (location, polartype, i) ->
      Int (location, polartype, i)
    | Float (location, polartype, f) ->
      Float (location, polartype, f)
    | String (location, polartype, s) ->
      String (location, polartype, s)
    | Unit (location, polartype) ->
      Unit (location, polartype)
    | Variable (location, polartype, name) ->
      Variable (location, polartype, name)
    | Field (location, polartype, receiver, name) ->
      Field (location, polartype, polart_iter receiver, name)
    | Object (location, polartype, fields) ->
      Object (location, polartype, object_polart_iter fields)
  in
  callback () ; return_value

let type_of = function
  | If (_, polartype, _, _, _) ->
    polartype
  | For (_, polartype, _, _, _, _) ->
    polartype
  | Apply (_, polartype, _, _) ->
    polartype
  | Definition (_, polartype, _, _) ->
    polartype
  | Block (_, polartype, _, _) ->
    polartype
  | Int (_, polartype, _) ->
    polartype
  | Float (_, polartype, _) ->
    polartype
  | String (_, polartype, _) ->
    polartype
  | Unit (_, polartype) ->
    polartype
  | Variable (_, polartype, _) ->
    polartype
  | Field (_, polartype, _, _) ->
    polartype
  | Object (_, polartype, _) ->
    polartype

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
  | For (_, polartype, initialization, condition, step, actions) ->
    Printf.sprintf
      "(%s | for %s ; %s ; %s { %s })"
      (string_of_polartype polartype)
      (string_of_polart initialization)
      (string_of_polart condition)
      (string_of_polart step)
      (String.concat " ; " (List.map string_of_polart actions))
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
        String.concat " " (List.map (fun (x, ptype) -> string_of_polartype ptype ^ "|" ^ x) arguments) ^
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
      Printf.sprintf "%s|%d"
        (string_of_polartype polartype)
        i
  | Float (_, polartype, f) ->
      Printf.sprintf "%s|%s"
        (string_of_polartype polartype)
        f
  | String (_, polartype, s) ->
      Printf.sprintf "%s|\"%s\""
        (string_of_polartype polartype)
        s
  | Unit (_, polartype) ->
      Printf.sprintf "%s|()"
        (string_of_polartype polartype)
  | Variable (_, polartype, name) ->
      Printf.sprintf "%s|%s"
        (string_of_polartype polartype)
        name
  | Field (_, polartype, receiver, name) ->
      "( " ^ (string_of_polartype polartype) ^ " | " ^ string_of_polart receiver ^ "." ^ name ^ ")"
  | Object (_, polartype, fields) ->
      "| " ^ string_of_polartype polartype ^ " < " ^
      String.concat " ; " (List.map (fun (name, e) -> name ^ " = " ^ string_of_polart e) fields) ^
      " >"
