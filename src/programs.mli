type id

module IdMap : Map.S with type key = id
module IdSet : Set.S with type elt = id

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

val naive_eval : program -> int64 -> int64
