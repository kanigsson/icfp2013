open Ast
open Programs

(*
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
  let size = 30 in
  let fold_kind = Program_generator.Top_fold (* No_fold *) in
  let generator = Program_generator.make unops binops fold_kind size in
  let p = generator () in
    Format.printf "%a@." print_program p;
  Format.printf "%d = %d"  size (Programs.size p);
  ()
*)

(*
let _ =
  Webapi.train 10
*)

let x = ref 0

let rec find_program input output gen =
  incr x;
  let p = gen () in
  if Programs.validates_constraints input output p then p
  else find_program input output gen

let _ =
  let p = Programs.program_of_file "training_programs/exp6.p" in
  let p = Programs.scoping p in
  let input = Arguments.int_64_arguments in
  let out = Array.map (Programs.eval p) input in
  let unops = [|Ast.Shl1; Ast.Shr4|] in
  let binops = [|Ast.Or|] in
  let fold = Program_generator.Top_fold in
  let size = 10 in
  let gen =
    Program_generator.make unops binops fold size in
  let p = find_program input out gen in
  Format.printf "%a@." Programs.print_program p;
  Format.printf "%d@." !x


