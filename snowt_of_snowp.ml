
(*
 * This file holds the code for converting
 * between Snowp trees and Snowt trees.
 * *)

let rec snowt_of_snowps snowps = List.map snowt_of_snowp snowps
and snowt_of_object_fields = function
  | [] -> []
  | ((a , (b : Snowp.snowp)) :: rest) -> (a, snowt_of_snowp b) :: snowt_of_object_fields rest
and snowt_of_snowp (snowp : Snowp.snowp) : Snowt.snowt =
  match snowp with
  | Snowp.If (location, condition, ifbranch, Some elsebranch) ->
    Snowt.If (
      location,
      snowt_of_snowp condition,
      snowt_of_snowps ifbranch,
      Some (snowt_of_snowps elsebranch) )
  | Snowp.If (location, condition, ifbranch, None) ->
    Snowt.If (
      location,
      snowt_of_snowp condition,
      snowt_of_snowps ifbranch,
      None )
  | Snowp.For (location, initialization, condition, step, actions) ->
    Snowt.For (
      location,
      snowt_of_snowp initialization,
      snowt_of_snowp condition,
      snowt_of_snowp step,
      snowt_of_snowps actions )
  | Snowp.Apply (location, f, argument) ->
    Snowt.Apply (
      location,
      snowt_of_snowp f,
      snowt_of_snowp argument )
  | Snowp.Definition (location, name, e) ->
    Snowt.Definition (
      location,
      name,
      snowt_of_snowp e )
  | Snowp.Block (location, arguments, snowps) ->
    Snowt.Block (
      location,
      arguments,
      snowt_of_snowps snowps )
  | Snowp.Int (location, i) ->
    Snowt.Int (
      location,
      i )
  | Snowp.Float (location, f) ->
    Snowt.Float (
      location,
      f )
  | Snowp.String (location, s) ->
    Snowt.String (
      location,
      s )
  | Snowp.Unit location ->
    Snowt.Unit (
      location )
  | Snowp.Variable (location, name) ->
    Snowt.Variable (
      location,
      name )
  | Snowp.Operator (location, operator_type, a, b) ->
      (*
       * This is where we convert operators to functions.
       * It makes sense to have operators information as a
       * parsing tree - not in a tree for type inference
       * and assertion.
       * *)
      snowt_of_snowp (Snowp.function_of_operator location operator_type a b)
  | Snowp.UOperator (location, operator_type, e) ->
      (* same for unary operators as it is for binary operators *)
      snowt_of_snowp (Snowp.function_of_uoperator location operator_type e)
  | Snowp.Field (location, receiver, name) ->
    Snowt.Field (
      location,
      snowt_of_snowp receiver,
      name)
  | Snowp.Object (location, fields) ->
    Snowt.Object (
      location,
      snowt_of_object_fields fields)
