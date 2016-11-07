
let compile_hazelnut (source : string) =
  let lexed_source = Lexing.from_string source in
  try
  let hzlts = Parser.hzlt Lexer.token lexed_source in
  List.map Hzlt.string_of_hzlt hzlts |> String.concat "\n"
  with Parser.Error ->
      Printf.sprintf "%d: syntax error.\n%!" (Lexing.lexeme_start lexed_source)

let all_input =
  let all = ref [] in
  try
    while true do
      let line = input_line stdin in
      all := line :: !all
    done; "this will never be returned"
  with End_of_file -> String.concat "\n" (List.rev !all)


let () =
  print_endline (compile_hazelnut all_input) 

