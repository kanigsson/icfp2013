open Ast

type eval_response =
  | Eval_ok of int64 array
  | Eval_error of string

val eval_response_of_string : string -> eval_response

type guess_response =
  | Guess_win
  | Guess_mismatch of int64 * int64 * int64
  | Guess_error of string

val guess_response_of_string : string -> guess_response

val problem_of_string : string -> problem

val my_problems_of_file : string -> problem list
