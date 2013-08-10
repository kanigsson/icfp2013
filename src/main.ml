open Ast
open Programs

let test_parse_and_eval () =
  let p = program_of_file Sys.argv.(1) in
  let p = scoping p in
  let v = eval p Int64.zero in
  let naive_v = naive_eval p Int64.zero in
  assert (v = naive_v);
  Format.printf "input : %s@." p.input.name

let test_generator () =
  let unops =
    [| Not;
       Shl1;
       Shr1;
       Shr4;
       Shr16; |]
  in
  let binops =
    [| And;
       Or;
       Xor;
       Plus; |]
  in
  let generator =
    Program_generator.make unops binops Program_generator.No_fold 30
  in
  Format.printf "%a@." print_program (generator ())


let _ =
  print_endline "hello";
  let x = Webapi.my_problems () in
  x
(*
  Random.self_init ();
  test_generator ()
*)
