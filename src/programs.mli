open Ast

val program_of_file : string -> string program
val program_of_string : string -> string program

val scoping : string program -> id program

val apply_unop : unop -> int64 -> int64
val apply_binop : binop -> int64 -> int64 -> int64
val naive_eval : id program -> int64 -> int64
val eval : id program -> int64 -> int64
val size : 'a program -> int

val int64_to_hex_string : int64 -> string

val print_program : Format.formatter -> id program -> unit

val validates_constraints : int64 array -> int64 array -> id program -> bool

val expr_map_child : ('a expr -> 'a expr) -> 'a expr -> 'a expr

val simplify_expr : 'a expr -> 'a expr
