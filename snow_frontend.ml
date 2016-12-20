
(* snow test frontend *)

open Interact

let () =
  let snowps = parse_snow all_input in
  List.map Snowp.string_of_snowp snowps |> List.iter print_endline

