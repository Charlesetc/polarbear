
(*
 * This file holds the code for converting
 * between Polarp trees and Polart trees.
 * *)

let rec polart_of_polarps polarps = List.map polart_of_polarp polarps
and polart_of_object_fields = function
  | [] -> []
  | ((a , (b : Polarp.polarp)) :: rest) -> (a, polart_of_polarp b) :: polart_of_object_fields rest
and polart_of_polarp (polarp : Polarp.polarp) : Polart.polart =
  match polarp with
  | Polarp.If (location, condition, ifbranch, Some elsebranch) ->
    Polart.If (
      location,
      Polart.initial_type (),
      polart_of_polarp condition,
      polart_of_polarps ifbranch,
      Some (polart_of_polarps elsebranch) )
  | Polarp.If (location, condition, ifbranch, None) ->
    Polart.If (
      location,
      Polart.initial_type (),
      polart_of_polarp condition,
      polart_of_polarps ifbranch,
      None )
  | Polarp.For (location, initialization, condition, step, actions) ->
    Polart.For (
      location,
      Polart.initial_type (),
      polart_of_polarp initialization,
      polart_of_polarp condition,
      polart_of_polarp step,
      polart_of_polarps actions )
  | Polarp.Apply (location, f, argument) ->
    Polart.Apply (
      location,
      Polart.initial_type (),
      polart_of_polarp f,
      polart_of_polarp argument )
  | Polarp.Definition (location, name, e) ->
    Polart.Definition (
      location,
      Polart.initial_type (),
      name,
      polart_of_polarp e )
  | Polarp.Block (location, arguments, polarps) ->
    Polart.Block (
      location,
      Polart.initial_type (),
      arguments,
      polart_of_polarps polarps )
  | Polarp.Int (location, i) ->
    Polart.Int (
      location,
      Polart.initial_type (),
      i )
  | Polarp.Float (location, f) ->
    Polart.Float (
      location,
      Polart.initial_type (),
      f )
  | Polarp.String (location, s) ->
    Polart.String (
      location,
      Polart.initial_type (),
      s )
  | Polarp.Unit location ->
    Polart.Unit (
      location,
      Polart.initial_type () )
  | Polarp.Variable (location, name) ->
    Polart.Variable (
      location,
      Polart.initial_type (),
      name )
  | Polarp.Operator (location, operator_type, a, b) ->
      (*
       * This is where we convert operators to functions.
       * It makes sense to have operators information as a
       * parsing tree - not in a tree for type inference
       * and assertion.
       * *)
      polart_of_polarp (Polarp.function_of_operator location operator_type a b)
  | Polarp.UOperator (location, operator_type, e) ->
      (* same for unary operators as it is for binary operators *)
      polart_of_polarp (Polarp.function_of_uoperator location operator_type e)
  | Polarp.Field (location, receiver, name) ->
    Polart.Field (
      location,
      Polart.initial_type (),
      polart_of_polarp receiver,
      name)
  | Polarp.Object (location, fields) ->
    Polart.Object (
      location,
      Polart.initial_type (),
      polart_of_object_fields fields)
