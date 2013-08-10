open Ast

type fold_kind =
  | No_fold
  | Top_fold
  | Inner_fold

val make :
    unop array -> binop array -> fold_kind -> int -> (unit -> id program)
