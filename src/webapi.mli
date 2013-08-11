open Ast

exception Error of int * string

type problem_id = string

val eval : problem_id -> int64 array -> int64 array

type problem =
  { id : problem_id;
    size : int;
    operators : string;
  }

val my_problems : unit -> unit

val train : int -> unit

val guess : problem_id -> id program -> Json.guess_response
