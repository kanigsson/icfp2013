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

type expr =
  | Const of int64
  | Var of id
  | If_Zero of expr * expr * expr
  | Unop of unop * expr
  | Binop of binop * expr * expr
  | Fold of expr * id * id * expr * expr

type program =
  { input : id; expr : expr }

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
        assert false
  in
  eval

let naive_eval p input =
  naive_eval_expr (IdMap.singleton p.input input) p.expr

