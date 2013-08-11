open Ast
open Yojson.Safe

let error x =
  Format.eprintf "error: %s@." (pretty_to_string x);
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
      | `String "win" -> Guess_win
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


let with_bonus_of_list l =
  List.mem "bonus" l

let with_if_of_list l =
  List.mem "if0" l

let fold_kind_of_list l =
  if List.mem "fold" l then Inner_fold
  else if List.mem "tfold" l then Top_fold
  else No_fold

let unop_of_string s =
  match s with
  | "not" -> Some Not
  | "shl1" -> Some Shl1
  | "shr1" -> Some Shr1
  | "shr4" -> Some Shr4
  | "shr16" -> Some Shr16
  | _ -> None

let unop_of_list l =
  let ops =
    List.fold_left
      (fun acc x ->
        match unop_of_string x with
        | Some op -> op :: acc
        | None -> acc)
      [] l
  in
  Array.of_list ops

let binop_of_string s =
  match s with
  | "and" -> Some And
  | "or" -> Some Or
  | "xor" -> Some Xor
  | "plus" -> Some Plus
  | _ -> None

let binop_of_list l =
  let ops =
    List.fold_left
      (fun acc x ->
        match binop_of_string x with
        | Some op -> op :: acc
        | None -> acc)
      [] l
  in
  Array.of_list ops


let problem_of_json x =
  match x with
  | `Assoc l ->
      let id =
        begin match find_assoc "id" l with
        | `String s -> s
        | x -> error x
        end
      in
      let size =
        begin match find_assoc "size" l with
        | `Int i -> i
        | `Intlit s | `String s -> int_of_string s
        | x -> error x
        end
      in
      let operators =
        begin match find_assoc "operators" l with
        | `List l ->
            List.map
              (fun x ->
                match x with
                | `String x -> x
                | x -> error x)
              l
        | x -> error x
        end
      in

      { pb_id = id;
        pb_size = size;
        pb_with_if = with_if_of_list operators;
        pb_fold_kind = fold_kind_of_list operators;
        pb_unop = unop_of_list operators;
        pb_binop = binop_of_list operators; }
  | x -> error x

let problem_of_string x =
  problem_of_json (from_string x)

let my_problems_of_file f =
  let c = open_in f in
  match from_channel c with
  | `List l ->
      List.map problem_of_json l
  | x -> error x
