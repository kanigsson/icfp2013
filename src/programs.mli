open Ast

val scoping : string program -> id program

val naive_eval : id program -> int64 -> int64
val eval : id program -> int64 -> int64

val int64_to_hex_string : int64 -> string
