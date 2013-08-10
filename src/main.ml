open Ast
open Programs

let test_parse_and_eval () =
  let p = program_of_file Sys.argv.(1) in
  let p = scoping p in
  let v = eval p Int64.zero in
  let naive_v = naive_eval p Int64.zero in
  assert (v = naive_v);
  Format.printf "input : %s@." p.input.name

let _ =
  print_endline "hello";
  let x = Webapi.eval "1" (Array.map Int64.of_int [|12;13|]) in
  for i = 0 to Array.length x - 1 do
    print_endline (Int64.to_string x.(i))
  done


