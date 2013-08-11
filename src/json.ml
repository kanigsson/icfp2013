open Yojson.Safe

let error x =
  Format.eprintf "%s@." (pretty_to_string x);
  assert false

let find_assoc x l =
  try List.assoc x l
  with Not_found ->
    Format.eprintf "Json.find_assoc %s" x;
    assert false

let int64_of_json x =
  match x with
  | `String v | `Intlit v -> Int64.of_string v
  | `Int v -> Int64.of_int v
  | _ -> error x

let find_string x l =
  match find_assoc x l with
  | `String s -> s
  | _ -> assert false

let find_int x l =
  match find_assoc x l with
  | `Int v -> v
  | _ -> assert false

type eval_response =
  | Eval_ok of int64 array
  | Eval_error of string

let eval_response_of_string s =
  match from_string s with
  | `Assoc l ->
      begin match find_assoc "status" l with
      | `String "ok" ->
          begin match find_assoc "outputs" l with
          | `List l ->
              let l = List.map int64_of_json l in
              Eval_ok (Array.of_list l)
          | x -> error x
          end
      | `String "error" ->
          begin match find_assoc "message" l with
          | `String msg -> Eval_error msg
          | x -> Eval_error (pretty_to_string x)
          end
      | x -> error x
      end
  | x -> error x

type guess_response =
  | Guess_win
  | Guess_mismatch of int64 * int64 * int64
  | Guess_error of string

let guess_response_of_string s =
  match from_string s with
  | `Assoc l ->
      begin match find_assoc "status" l with
      | `String "ok" -> Guess_win
      | `String "mismatch" ->
          begin match find_assoc "values" l with
          | `List l ->
              begin match List.map int64_of_json l with
              | [v1; v2; v3] -> Guess_mismatch (v1, v2, v3)
              | _ -> error (from_string s)
              end
          | x -> error x
          end
      | `String "error" ->
          begin match find_assoc "message" l with
          | `String msg -> Guess_error msg
          | x -> Guess_error (pretty_to_string x)
          end
      | x -> error x
      end
  | x -> error x

type problem 
let parse_problem x =
  match x with
  | `Assoc l ->
      let id = find_string "id" l in
      let size = find_int "size" l in


let my_problems s =
  match from_string s with
  | `List l ->
      List.map parse_problem l
  | _ -> assert false
