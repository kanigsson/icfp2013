open Yojson.Safe

let error x =
  Format.eprintf "%s@." (pretty_to_string x);
  assert false

let find_assoc x l =
  try List.assoc x l
  with Not_found ->
    Format.eprintf "Json.find_assoc %s" x;
    assert false

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
              let l =
                List.map
                  (fun x ->
                    match x with
                    | `String v | `Intlit v -> Int64.of_string v
                    | `Int v -> Int64.of_int v
                    | _ -> error x)
                  l
              in
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
