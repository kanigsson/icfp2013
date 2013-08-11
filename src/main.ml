open Ast
open Programs

let size, args =
  let seed = ref (-1) in
  let size = ref 15 in
  let rev_args = ref [] in
  Arg.parse
    ["-seed", Arg.Set_int seed,
     "Set the seed of the random generator" ;
     "-size", Arg.Set_int size,
     "Set the size of the generated programs for the benchs" ; ]
    (fun x -> rev_args := x :: !rev_args)
    "options:";
  let () =
    if !seed = -1 then Random.self_init ()
    else Random.init !seed
  in
  !size, List.rev !rev_args

(*
let test_parse_and_eval () =
  let p = program_of_file Sys.argv.(1) in
  let p = scoping p in
  let v = eval p Int64.zero in
  let naive_v = naive_eval p Int64.zero in
  assert (v = naive_v);
  Format.printf "input : %s@." p.input.name
*)


let _ =
  Webapi.train 10

(*
let x = ref 0

let rec find_program input output gen =
  incr x;
  let p = gen () in
  if Programs.validates_constraints input output p then p
  else find_program input output gen



let () =
  (* Bench.generation size Program_generator.No_fold 1_000_000; *)
  (* Bench.find_programs size Program_generator.No_fold 75; *)
  ()
*)

(*
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
*)


let _ = Json.eval_response_of_string "{ \"status\":\"ok\",\"outputs\":[\"YOLCg1BdhPGmrXVwwxnoFQ7b\",\"0x0000000000000001\",\"0x0000000000000002\"]}"
