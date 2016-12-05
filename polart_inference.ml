
(*
 * This file defines the algorithm that
 * takes a polart and adds the appropriate
 * non-generic types to it.
 * *)


let limiter f polart limits =
  let limits = ref limits in
  let limit value = limits := value :: !limits in
  ignore @@ Polart.polart_iter (function x  -> match f x with
    | Some x -> limit x ; ignore
    | None -> ignore
  ) polart ;
  !limits

type limit = {
  first : Polart.polartype ;
  second : Polart.polartype ;
  location : Polarp.location ;
}

let string_of_limit limit =
  Polart.string_of_polartype limit.first ^ " => " ^ Polart.string_of_polartype limit.second

let get_id (polartype : Polart.polartype) : string = match polartype with
  | Polart.Generic a -> a
  | _ -> raise Not_found

let application_limits =
  limiter @@ function
    | Polart.Apply (location, type1, f, argument) ->
        Some {
          first = f |> Polart.type_of ;
          second = Polart.Function (Polart.type_of argument, type1) ;
          location = location ;
        }
    | _ -> None

let definition_limits =
  limiter @@ function
    | Polart.Definition (location, polartype, name, value) ->
        Some {
          first = polartype ;
          second = Polart.type_of value ;
          location = location ;
        }
    | _ -> None

let block_limits =
  limiter @@ function
    | Polart.Block (location, polartype, arguments, actions) ->
        let rec function_from_arguments xs = match xs with
          | [] ->
              List.nth actions (List.length actions - 1) |> Polart.type_of
          | (_, x)::xs -> Polart.Function (x, function_from_arguments xs)
        in
        Some {
          first = polartype ;
          second = function_from_arguments arguments ;
          location = location ;
        }
    | _ -> None

let equivalence_limits polart limits =
  let limits = ref limits in
  let limit value = limits := value :: !limits in

  let alias_table = Hashtbl.create 100 in

  let equivalence_f = function
    | Polart.Definition (location, polartype, name, value) ->
        let existing = try Hashtbl.find alias_table name with Not_found -> [] in
        Hashtbl.add alias_table name ((polartype, location) :: existing) ;
        (function () -> Hashtbl.add alias_table name existing)
    | Polart.Block (loc, _, arguments, actions) ->

        (* This piece of code is basically the same as that one above but it's lifted
         * to work for a bunch of arguments, not just a single value.
         * Feel free to make this a function later on.
         * *)

        List.iter ( function (name, ptype) ->
          let existing = try Hashtbl.find alias_table name with Not_found -> [] in
          Hashtbl.add alias_table name ((ptype, loc) :: existing)
        ) arguments ;
        (function () ->
          List.iter
            (function (name, _) ->
              let existing = try Hashtbl.find alias_table name with Not_found -> [] in
              Hashtbl.add alias_table name existing)
            arguments)
    | Polart.Variable (_, polartype, name) ->
      (try
        let (id_type, loc) = Hashtbl.find alias_table name |> List.hd in
        limit {
          first = id_type ;
          second = polartype ;
          location = loc ;
        } ; ignore
      with
        | Not_found -> ignore
        | Failure "hd" -> ignore)
    | _ -> ignore
  in

  ignore @@ Polart.polart_iter equivalence_f polart ;
  !limits

let infer_types polart =
  let limits = [] in
  let limits = application_limits polart limits in
  let limits = definition_limits polart limits in
  let limits = block_limits polart limits in
  let limits = equivalence_limits polart limits in
  limits |> List.map string_of_limit |> List.map print_endline |> ignore ;
  polart
