type eval_response =
  | Eval_ok of string * int64 * int64
  | Eval_error of string

val eval_response_of_string : string -> eval_response
