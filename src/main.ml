open Programs

let program_of_file f =
  let c = open_in f in
  let lb = Lexing.from_channel c in
  Parser.file Lexer.token lb

let _ =
  print_endline "hello";
  let x = Webapi.eval "1" (Array.map Int64.of_int [|12;13|]) in
  for i = 0 to Array.length x - 1 do
    print_endline (Int64.to_string x.(i))
  done
  (* let p = program_of_file Sys.argv.(1) in *)
  (* Format.printf "input : %s@." p.input *)

