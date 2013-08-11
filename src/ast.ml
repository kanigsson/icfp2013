type id =
    { name : string;
      mutable value : Int64.t;
      id : int; }

let gen_id =
  let cpt = ref 0 in
  (fun x -> incr cpt; { name = x; value = Int64.zero; id = !cpt; })

let print_id fmt x =
  Format.fprintf fmt "%s_%d" x.name x.id

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



type fold_kind =
  | No_fold
  | Top_fold
  | Inner_fold

type problem =
    { pb_id: string;
      pb_size: int;
      pb_unop: unop array;
      pb_binop: binop array;
      pb_fold_kind: fold_kind;
      pb_with_if: bool;
      pb_solved: bool option; }


let string_of_unop op =
  match op with
  | Not -> "not"
  | Shl1 -> "shl1"
  | Shr1 -> "shr1"
  | Shr4 -> "shr4"
  | Shr16 -> "shr16"

let string_of_binop op =
  match op with
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"
  | Plus -> "plus"

let string_of_fold_kind k =
  match k with
  | No_fold -> "no fold"
  | Top_fold -> "tfold"
  | Inner_fold -> "fold"


let print_problem fmt p =
  Format.fprintf fmt "{ @[id = %s;@\n" p.pb_id;
  Format.fprintf fmt "size = %d;@\n" p.pb_size;
  Format.fprintf fmt "unops = [ ";
  Array.iter (fun op -> Format.fprintf fmt "%s; " (string_of_unop op)) p.pb_unop;
  Format.fprintf fmt "]@\n";
  Format.fprintf fmt "binops = [ ";
  Array.iter (fun op -> Format.fprintf fmt "%s; " (string_of_binop op)) p.pb_binop;
  Format.fprintf fmt "]@\n";
  Format.fprintf fmt "fold_kind = %s;@\n" (string_of_fold_kind p.pb_fold_kind);
  Format.fprintf fmt "with_if = %s;@\n" (string_of_bool p.pb_with_if);
  Format.fprintf fmt "solved = %s;@\n"
    (match p.pb_solved with
    | None -> "?"
    | Some b -> string_of_bool b);
  Format.fprintf fmt "@] }@\n";
  ()
