type id =
    { name : string;
      mutable value : Int64.t;
      id : int; }

val gen_id : string -> id

val print_id : Format.formatter -> id -> unit

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

type 'id expr =
  | Const of int64
  | Var of 'id
  | If_Zero of 'id expr * 'id expr * 'id expr
  | Unop of unop * 'id expr
  | Binop of binop * 'id expr * 'id expr
  | Fold of 'id expr * 'id expr * 'id * 'id * 'id expr

type 'id program =
  { input : 'id; expr : 'id expr }
