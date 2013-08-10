open Ast

type fold_kind =
  | No_fold
  | Top_fold
  | Inner_fold

let make unops binops fold_kind size =
  let nb_unops = Array.length unops in
  let nb_binops = Array.length binops in


  let input = gen_id "x" in
  let v = gen_id "v" in
  let acc = gen_id "acc" in
  let vars =
    match fold_kind with
    | No_fold -> [| Var input |]
    | Top_fold | Inner_fold -> [| Var input; Var v ; Var acc; |]
  in
  let exprs_size_1 =
    Array.append
      [| Const Int64.zero;
         Const Int64.one; |]
      vars
  in
  let nb_size_1 = Array.length exprs_size_1 in

  let nb_size_2 = nb_unops * nb_size_1 in
  let exprs_size_2 =
    let tmp = Array.make nb_size_2 (Const Int64.zero) in
    let k = ref 0 in
    for i = 0 to nb_unops - 1 do
      for j = 0 to nb_size_1 - 1 do
        tmp.(!k) <- Unop (unops.(i), exprs_size_1.(j));
        incr k
      done
    done;
    tmp
  in

  let rec random_expr size =
    match size with
    | 1 -> random_value ()
    | 2 -> random_unop 2
    | 3 ->
        if Random.int 100 < 50 then
          random_unop 3
        else
          random_binop 3
    | n ->
        if Random.int 100 < 50 then
          random_unop n
        else (* if Random.int 100 < 50 then *)
          random_binop n

  and random_value () =
    exprs_size_1.(Random.int nb_size_1)

  and random_unop size =
    if size = 2 then
      exprs_size_2.(Random.int nb_size_2)
    else
      Unop (unops.(Random.int nb_unops), random_expr (size - 1))

  and random_binop size =
    let size = size - 1 in
    let size1 =
      if size = 2 then 1
      else Random.int (size - 2) + 1
    in
    let size2 = size - size1 in
    let size_left, size_right =
      (* Comme tous les opérateurs binaires sont commutatifs,
         on peut representer toutes les fonctions en respectant
         l'invariant que size_left < size_right. *)
      if size1 < size2 then (size1, size2) else (size2, size1)
    in
    Binop (binops.(Random.int nb_binops),
           random_expr size_left,
           random_expr size_right)
  in
  fun () ->
    match fold_kind with
    | No_fold ->
        { input = input; expr = random_expr (size - 1); }
    | Top_fold ->
        let e = random_expr (size - 5) in
        { input = input; expr = Fold (Var input, Const Int64.zero, acc, v, e); }
    | Inner_fold ->
        failwith "Program_generator: TODO"
