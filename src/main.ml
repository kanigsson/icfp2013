open Programs

let program_of_file f =
  let c = open_in f in
  let lb = Lexing.from_channel c in
  Parser.file Lexer.token lb

let _ =
  print_endline "hello";
  Webapi.test ()
  (* let p = program_of_file Sys.argv.(1) in *)
  (* Format.printf "input : %s@." p.input *)

