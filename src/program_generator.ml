open Ast

type fold_kind =
  | No_fold
  | Top_fold
  | Inner_fold

let make unops binops fold size =
  let input = gen_id "x" in
  let vars =
    let v = gen_id "v" in
    let acc = gen_id "acc" in
    match fold with
    | No_fold -> [| Var input |]
    | Top_fold | Inner_fold -> [| Var input; Var v ; Var acc; |]
  in
  let exprs_size_1 =
    Array.append
      [| Const Int64.zero;
         Const Int64.one; |]
      vars
  in
  let nb_unops = Array.length unops in
  let nb_binops = Array.length binops in
  let nb_size_1 = Array.length exprs_size_1 in

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
    Unop (unops.(Random.int nb_unops), random_expr (size - 1))

  and random_binop size =
    let size = size - 1 in
    let size_left =
      if size = 2 then 1
      else Random.int (size - 2) + 1
    in
    let size_right = size - size_left in
    Binop (binops.(Random.int nb_binops),
           random_expr size_left,
           random_expr size_right)
  in
  fun () ->
    { input = input; expr = random_expr (size - 1); }
