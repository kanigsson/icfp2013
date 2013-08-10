type id =
    { name : string;
      mutable value : Int64.t;
      id : int; }

let gen_id =
  let cpt = ref 0 in
  (fun x -> incr cpt; { name = x; value = Int64.zero; id = !cpt; })

module IdMap = Map.Make (struct
  type t = id
  let compare id1 id2 = compare id1.id id2.id
end)
module IdSet = Set.Make (struct
  type t = id
  let compare id1 id2 = compare id1.id id2.id
end)

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
