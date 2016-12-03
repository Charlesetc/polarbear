
(* polarbear main *)

open Interact

let () =
  let polarps = compile_polarbear all_input in
  (polarps : Polarp.polarp list)
    |> Polart_of_polarp.polart_of_polarps
    |> List.map Polart.string_of_polart
    |> List.iter print_endline
