open Ast

val make :
    unop array -> binop array -> fold_kind -> bool -> int -> (unit -> id program)
