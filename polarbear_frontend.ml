
(* polarbear main *)

open Interact

let () =
  let polarps = compile_polarbear all_input in
  List.map Polarp.string_of_polarp polarps |> List.iter print_endline

