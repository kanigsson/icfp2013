open Ast
open Programs

let force_size = ref None

let print_64_array x =
  Format.printf "[|";
  for i = 0 to Array.length x - 1 do
    Format.printf "%s;" (Programs.int64_to_hex_string x.(i))
  done;
  Format.printf "|]"

let rec find_program input output gen =
  let p = gen () in
  if Programs.validates_constraints input output p then p
  else find_program input output gen

let run_guesser problem =
  let gen =
    Program_generator.make
      problem.pb_unop
      problem.pb_binop
      problem.pb_fold_kind
      problem.pb_with_if
      (match !force_size with
      | None -> problem.pb_size
      | Some s -> s)
  in
  let input = Arguments.int_64_arguments in
  let out = Webapi.eval problem.pb_id input in
  Format.printf "Inputs (%d):@[" (Array.length input);
  print_64_array input;
  Format.printf "@]@.";
  Format.printf "Outpouts:@[";
  print_64_array out;
  Format.printf "@]@.";
  let rec run input output =
    Format.printf "obtained eval@.";
    let p = find_program input output gen in
    Format.printf "found program: @[%a@]@." print_program p;
    let gr = Webapi.guess problem.pb_id p in
    Format.printf "guessed problem@.";
    match gr with
    | Json.Guess_win ->
        Format.printf "guessed program %s@." problem.pb_id
    | Json.Guess_mismatch (in_,out_corr, old_) ->
        Format.printf "incorrect for %s,%s,%s@."
          (int64_to_hex_string in_) (int64_to_hex_string out_corr)
          (int64_to_hex_string old_);
        run (Array.append input [|in_|]) (Array.append output [|out_corr|])
    | Json.Guess_error s ->
        Format.printf "guess error: %s" s;
        assert false
  in
  run input out

let first_problem =
  { pb_id = "2DjZA7zt9wyrobpCB2bA0X8x";
    pb_size = 3;
    pb_unop = [| Shr16 |];
    pb_binop = [| |];
    pb_fold_kind = No_fold;
    pb_with_if = false
  }



let problem, args =
  let seed = ref (-1) in
  let size = ref 15 in
  let rev_args = ref [] in
  let pb = ref "" in
  let train = ref (-1) in
  let my_problems_file = ref "" in
  let pb_id = ref "" in
  Arg.parse
    ["-pb", Arg.Set_string pb,
     "Parse and solve the problem given in argument";
     "-myproblems", Arg.Set_string my_problems_file,
     "Load the problem file";
     "-pb_id", Arg.Set_string pb_id,
     "Solve the problem given in argument from myproblems";
     "-train", Arg.Set_int train,
     "Ask a new test";
     "-seed", Arg.Set_int seed,
     "Set the seed of the random generator" ;
     "-size", Arg.Set_int size,
     "Force the size of the generated programs" ;]
    (fun x -> rev_args := x :: !rev_args)
    "options:";
  let () =
    if !seed = -1 then Random.self_init ()
    else Random.init !seed
  in
  let () =
    if !train > 0 then (Webapi.train !train; exit 0)
  in
  let () =
    if !size > 0 then force_size := Some !size
  in
  let my_problems =
    if !my_problems_file = "" then []
    else Json.my_problems_of_file !my_problems_file
  in
  let pb =
    if !pb <> "" then
      Json.problem_of_string !pb
    else if !pb_id <> "" then
      begin try
        let id = !pb_id in
        List.find (fun p -> p.pb_id = id) my_problems
      with Not_found ->
        Format.eprintf "Problem %s not found@." !pb_id;
        exit 1
      end
    else begin
      Format.eprintf "Nothinng to do@.";
      exit 0
    end
  in
  pb, List.rev !rev_args

let _ =
  Format.printf "Problem : @[%a@]@." print_problem problem;
  run_guesser problem;
  ()

(*


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
