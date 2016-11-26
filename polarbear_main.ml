
(* polarbear main *)

let compile_polarbear (source : string) =
  let lexed_source = Lexing.from_string source in
  try
    let polarts = Parser.polart Lexer.token lexed_source in
    let polarts = List.rev polarts in
    (* Polart.string_of_polart polarts *)
    List.map Polart.string_of_polart polarts |> String.concat "\n"
  with Parser.Error ->
    Printf.eprintf "%d to %d: syntax error.\n%!" (Lexing.lexeme_start lexed_source) (Lexing.lexeme_end lexed_source) ;
    exit 1

let all_input =
  let all = ref [] in
  try
    while true do
      let line = input_line stdin in
      all := line :: !all
    done; "this will never be returned"
  with End_of_file -> String.concat "\n" (List.rev !all) ^ "\n"


let () =
  print_endline (compile_polarbear all_input)

