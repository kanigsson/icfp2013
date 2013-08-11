open Yojson.Safe

let find_assoc x l =
  try List.assoc x l
  with Not_found ->
    Format.eprintf "Json.find_assoc %s" x;
    assert false

type eval_response =
  | Eval_ok of string * int64 * int64
  | Eval_error of string

let eval_response_of_string s =
  match from_string s with
  | `Assoc l ->
      begin match find_assoc "status" l with
      | `String "ok" ->
          begin match find_assoc "outputs" l with
          | `List [ `String id; `String output; `String input; ]
          | `List [ `String id; `Intlit output; `Intlit input; ] ->
              Eval_ok (id, Int64.of_string output, Int64.of_string input)
          | x ->
              Format.eprintf "%s@." (pretty_to_string x);
              assert false
          end
      | `String "error" -> (* XXX TODO XXX *) assert false
      | _ -> assert false
      end
  | _ -> assert false
