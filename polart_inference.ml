
let constrainer f polart = 
  let constraints = ref [] in
  let constrain value = constraints := value :: !constraints in
  ignore @@ Polart.polart_iter (function x -> match f x with
    | Some x -> constrain x
    | None -> ()
  ) polart ;
  !constraints


type limitation = {
  id : string ;
  polartype : Polart.polartype ;
  location : Polarp.location ;
}

let string_of_limitation limitation =
  limitation.id ^ " => " ^ Polart.string_of_polartype limitation.polartype

let get_id (polartype : Polart.polartype) : string = match polartype with
  | Polart.Generic a -> a
  | _ -> raise Not_found

let application_constraints =
  constrainer @@ function
    | Polart.Apply (location, type1, f, argument) ->
        Some {
          id = get_id type1 ;
          polartype = Polart.Function (Polart.type_of f, Polart.type_of argument) ;
          location = location ;
        }
    | _ -> None

let infer_types polart =
  let constraints = application_constraints polart in
  constraints |> List.map string_of_limitation |> List.map print_endline |> ignore ;
  polart
