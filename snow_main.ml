
(* snow main *)

open Interact

let () =
  let snowps = parse_snow all_input in
  (snowps : Snowp.snowp list)
    |> Snowt_of_snowp.snowt_of_snowps
    |> List.map Snowt.string_of_snowt
    |> List.iter print_endline
