
type location = {
  column : int ;
  row : int ;
}

let null_location = { column = 0 ; row = 0 }

type identifier = location * string

type hzlt
  = Let of location
    (* identifier assigned to *)
    * identifier
    (* expression *)
    * hzlt
  | If of location
    (* condition *)
    * hzlt
    (* true branch *)
    * hzlt
    (* optional false branch *)
    * hzlt option
  | For of location
    (* initialization *)
    * hzlt
    (* condition *)
    * hzlt
    (* step *)
    * hzlt
    (* action *)
    * hzlt
  | Apply of location
    (* function *)
    * hzlt
    (* argument *)
    * hzlt
  | Definition of location
    (* name of the function *)
    * identifier
    (* argument list *)
    * identifier list
    (* body *)
    * hzlt
  | Int of location * int
  | Float of location
    (* string because it would be terrible
     * to get a precision error in the compiler.
     * *)
    * string
  | String of location * string
  | Unit of location
  | Variable of location * string

let string_of_identifier (_, s) = s

let rec string_of_hzlt hzlt =
  match hzlt with
  | Let (_, identifier, child) ->
      Printf.sprintf
        "(let %s = %s)"
        (string_of_identifier identifier)
        (string_of_hzlt child)
  | If (_, condition, ifbranch, Some elsebranch) ->
    Printf.sprintf
      "(if %s { %s } else { %s })"
      (string_of_hzlt condition)
      (string_of_hzlt ifbranch)
      (string_of_hzlt elsebranch)
  | If (_, condition, ifbranch, None) ->
    Printf.sprintf
      "(if %s { %s })"
      (string_of_hzlt condition)
      (string_of_hzlt ifbranch)
  | For (_, initialization, condition, step, action) ->
    Printf.sprintf
      "(for %s ; %s ; %s { %s })"
      (string_of_hzlt initialization)
      (string_of_hzlt condition)
      (string_of_hzlt step)
      (string_of_hzlt action)
  | Apply (_, f, argument) ->
    Printf.sprintf
      "(%s %s)"
      (string_of_hzlt f)
      (string_of_hzlt argument)
  | Definition (_, function_name, arguments, body) ->
    Printf.sprintf
      "(define %s %s { %s })"
      (string_of_identifier function_name)
      (String.concat " "
        (List.map string_of_identifier arguments))
      (string_of_hzlt body)
  | Int (_, i) ->
      Printf.sprintf "%d" i
  | Float (_, f) ->
      Printf.sprintf "%s" f
  | String (_, s) ->
      Printf.sprintf "\"%s\"" s
  | Unit _ ->
      "()"
  | Variable (_, name) ->
      Printf.sprintf "%s" name
