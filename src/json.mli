type eval_response =
  | Eval_ok of int64 array
  | Eval_error of string

val eval_response_of_string : string -> eval_response
