type id = int

module IdMap = Map.Make (struct type t = id let compare = compare end)
module IdSet = Set.Make (struct type t = id let compare = compare end)

type unop =
  | Not
  | Shl1
  | Shr1
  | Shr4
  | Shr16

type binop =
  | And
  | Or
  | Xor
  | Plus

type 'id expr =
  | Const of int64
  | Var of 'id
  | If_Zero of 'id expr * 'id expr * 'id expr
  | Unop of unop * 'id expr
  | Binop of binop * 'id expr * 'id expr
  | Fold of 'id expr * 'id expr * 'id * 'id * 'id expr

type 'id program =
  { input : 'id; expr : 'id expr }

let apply_unop op r =
  match op with
  | Not -> Int64.lognot r
  | Shl1 -> Int64.shift_left r 1
  | Shr1 -> Int64.shift_right r 1
  | Shr4 -> Int64.shift_right r 4
  | Shr16 -> Int64.shift_right r 16

let apply_binop op r1 r2 =
  match op with
  | And -> Int64.logand r1 r2
  | Or -> Int64.logor r1 r2
  | Xor -> Int64.logxor r1 r2
  | Plus -> Int64.add r1 r2

let naive_eval_expr =
  let rec eval map e =
    match e with
    | Const c -> c
    | Unop (op, e) ->
        let r = eval map e in
        apply_unop op r
    | Binop (op, e1, e2) ->
        let r1 = eval map e1 in
        let r2 = eval map e2 in
        apply_binop op r1 r2
    | If_Zero (e1,e2,e3) ->
        let r = eval map e1 in
        if r = Int64.zero then eval map e1 else eval map e2
    | Var v -> IdMap.find v map
    | Fold (e0,e1,x,y,e2) ->
        let v = eval map e0 in
        let acc = ref (eval map e1) in
        for i = 1 to 8 do
          let map = IdMap.add y !acc map in
          let input =
            Int64.logand (Int64.of_int 255) (Int64.shift_right v ((i-1)*8)) in
          let map = IdMap.add x input map in
          acc := eval map e2
        done;
        !acc
  in
  eval

let naive_eval p input =
  naive_eval_expr (IdMap.singleton p.input input) p.expr


let rec expr_size e =
  match e with
  | Const _ | Var _ -> 1
  | If_Zero (e1,e2,e3) -> 1 + expr_size e1 + expr_size e2 + expr_size e3
  | Unop (_,e) -> 1 + expr_size e
  | Binop (_,e1,e2) -> 1 + expr_size e1 + expr_size e2
  | Fold (e0,e1,_,_,e2) -> 2 + expr_size e2 + expr_size e2 + expr_size e0

let size p = 1 + expr_size p.expr
