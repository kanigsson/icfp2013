open Ast

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
  let values =
    Array.append
      [| Const Int64.zero;
         Const Int64.one; |]
      vars
  in
  let nb_size_1 = Array.length values in

  let nb_unop_size_2 = nb_unops * nb_size_1 in
  let unop_size_2 =
    let tmp = Array.make nb_unop_size_2 (Const Int64.zero) in
    let k = ref 0 in
    for i = 0 to nb_unops - 1 do
      for j = 0 to nb_size_1 - 1 do
        tmp.(!k) <- Unop (unops.(i), values.(j));
        incr k
      done
    done;
    tmp
  in

  let nb_binop_size_3 = nb_binops * (nb_size_1 * (nb_size_1 + 1) / 2) in
  let binop_size_3 =
    let tmp = Array.make nb_binop_size_3 (Const Int64.zero) in
    let k = ref 0 in
    for i = 0 to nb_binops - 1 do
      for j = 0 to nb_size_1 - 1 do
        for j' = j (* car assoc *) to nb_size_1 - 1 do
          let e = Binop (binops.(i), values.(j), values.(j')) in
          tmp.(!k) <- Programs.simplify_expr e;
          incr k
        done
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
    values.(Random.int nb_size_1)

  and random_unop size =
    if size = 2 then
      unop_size_2.(Random.int nb_unop_size_2)
    else
      Unop (unops.(Random.int nb_unops), random_expr (size - 1))

  and random_binop size =
    if size = 3 then
      binop_size_3.(Random.int nb_binop_size_3)
    else
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
        let expr = random_expr (size - 1) in
        { input = input; expr = Programs.simplify_expr expr; }
    | Top_fold ->
        let e = random_expr (size - 5) in
        let expr = Fold (Var input, Const Int64.zero, acc, v, e) in
        { input = input; expr = Programs.simplify_expr expr; }
    | Inner_fold ->
        failwith "Program_generator: TODO"
