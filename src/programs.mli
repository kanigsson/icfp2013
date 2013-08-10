open Ast

val scoping : string program -> id program

val naive_eval : id program -> int64 -> int64
