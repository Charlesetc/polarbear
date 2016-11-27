
(* polarbear main *)

open Interact

let () =
  let polarts = compile_polarbear all_input in
  List.map Polart.string_of_polart polarts |> List.iter print_endline

