open Ast

let unops =
  [| Not;
     Shl1;
     Shr1;
     Shr4;
     Shr16; |]
let binops =
  [| And;
     Or;
     Xor;
     Plus; |]

let generation size fold_kind n =
  let start_time = Sys.time () in
  let generate = Program_generator.make unops binops fold_kind size in
  for i = 1 to n do ignore (generate ()) done;
  let end_time = Sys.time () in
  Format.printf "temps = %fs (nb generation = %d)@."
    (end_time -. start_time)
    n

let rec find_programs input output gen n acc nb_try =
  if 0 < n && nb_try < 10_000_000 then
    let p = gen () in
    if Programs.validates_constraints input output p then
      find_programs input output gen (n - 1) (p :: acc) (nb_try + 1)
    else
      find_programs input output gen n acc (nb_try + 1)
  else
    (acc, nb_try)

let find_programs size fold_kind n =
  let start_time = Sys.time () in
  let generate = Program_generator.make unops binops fold_kind size in
  let p = generate () in
  Format.printf "p = %a@." Programs.print_program p;
  let input = Arguments.int_64_arguments in
  let out = Array.map (Programs.eval p) input in
  let sols, nb_try = find_programs input out generate n [] 0 in
  let end_time = Sys.time () in
  Format.printf "temps = %fs (nb essais = %d, nb solutions = %d)@."
    (end_time -. start_time)
    nb_try
    (List.length sols)
