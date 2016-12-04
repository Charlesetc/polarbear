
let limiter f polart limits =
  let limits = ref limits in
  let limit value = limits := value :: !limits in
  ignore @@ Polart.polart_iter (function x -> match f x with
    | Some x -> limit x
    | None -> ()
  ) polart ;
  !limits

exception Exception of string

type limit = {
  id : string ;
  polartype : Polart.polartype ;
  location : Polarp.location ;
}

let string_of_limit limit =
  limit.id ^ " => " ^ Polart.string_of_polartype limit.polartype

let get_id (polartype : Polart.polartype) : string = match polartype with
  | Polart.Generic a -> a
  | _ -> raise Not_found

let application_limits =
  limiter @@ function
    | Polart.Apply (location, type1, f, argument) ->
        Some {
          id = get_id type1 ;
          polartype = Polart.Function (Polart.type_of f, Polart.type_of argument) ;
          location = location ;
        }
    | _ -> None

let definition_limits =
  limiter @@ function
    | Polart.Definition (location, polartype, name, value) ->
        Some {
          id = get_id polartype ;
          polartype = Polart.type_of value ;
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
          id = get_id polartype ;
          polartype = function_from_arguments arguments ;
          location = location ;
        }
    | _ -> None

let infer_types polart =
  let limits = [] in
  let limits = application_limits polart limits in
  let limits = definition_limits polart limits in
  let limits = block_limits polart limits in
  limits |> List.map string_of_limit |> List.map print_endline |> ignore ;
  polart
