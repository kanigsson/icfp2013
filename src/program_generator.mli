open Ast

val make :
    unop array -> binop array -> fold_kind -> int -> (unit -> id program)
