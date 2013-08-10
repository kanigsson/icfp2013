open Ast

let program_of_file f =
  let c = open_in f in
  let lb = Lexing.from_channel c in
  Parser.file Lexer.token lb

let program_of_string s =
  let lb = Lexing.from_string s in
  Parser.file Lexer.token lb


let scoping p =
  let input = gen_id p.input in
  let rec scope env e =
    match e with
    | Const i -> Const i
    | Var id -> Var (List.assoc id env)
    | If_Zero (e, e1, e2) -> If_Zero (scope env e, scope env e1, scope env e2)
    | Unop (op, e) -> Unop (op, scope env e)
    | Binop (op, e1, e2) -> Binop (op, scope env e1, scope env e2)
    | Fold (e1, e2, x, y, e) ->
        let x' = gen_id x in
        let y' = gen_id y in
        let env' = (x,x') :: (y,y') :: env in
        Fold (scope env e1, scope env e2, x', y', scope env' e)
  in
  { input = input;
    expr = scope [p.input, input] p.expr; }

let apply_unop op r =
  match op with
  | Not -> Int64.lognot r
  | Shl1 -> Int64.shift_left r 1
  | Shr1 -> Int64.shift_right r 1
  | Shr4 -> Int64.shift_right r 4
  | Shr16 -> Int64.shift_right r 16

let apply_binop op r1 r2 =
  match op with
  | And -> Int64.logand r1 r2
  | Or -> Int64.logor r1 r2
  | Xor -> Int64.logxor r1 r2
  | Plus -> Int64.add r1 r2

let naive_eval_expr =
  let rec eval map e =
    match e with
    | Const c -> c
    | Unop (op, e) ->
        let r = eval map e in
        apply_unop op r
    | Binop (op, e1, e2) ->
        let r1 = eval map e1 in
        let r2 = eval map e2 in
        apply_binop op r1 r2
    | If_Zero (e1,e2,e3) ->
        let r = eval map e1 in
        if r = Int64.zero then eval map e1 else eval map e2
    | Var v -> IdMap.find v map
    | Fold (e0,e1,x,y,e2) ->
        let v = eval map e0 in
        let acc = ref (eval map e1) in
        for i = 1 to 8 do
          let map = IdMap.add y !acc map in
          let input =
            Int64.logand (Int64.of_int 255) (Int64.shift_right v ((i-1)*8)) in
          let map = IdMap.add x input map in
          acc := eval map e2
        done;
        !acc
  in
  eval

let naive_eval p input =
  naive_eval_expr (IdMap.singleton p.input input) p.expr


let eval_expr =
  let rec eval e =
    match e with
    | Const c -> c
    | Unop (op, e) ->
        let r = eval e in
        apply_unop op r
    | Binop (op, e1, e2) ->
        let r1 = eval e1 in
        let r2 = eval e2 in
        apply_binop op r1 r2
    | If_Zero (e1,e2,e3) ->
        let r = eval e1 in
        if r = Int64.zero then eval e1 else eval e2
    | Var v -> v.value
    | Fold (e0,e1,x,y,e2) ->
        let v = eval e0 in
        let acc = eval e1 in
        y.value <- acc;
        for i = 1 to 8 do
          let input =
            Int64.logand (Int64.of_int 255) (Int64.shift_right v ((i-1)*8))
          in
          x.value <- input;
          y.value <- eval e2
        done;
        y.value
  in
  eval

let eval p input =
  p.input.value <- input;
  eval_expr p.expr


let rec expr_size e =
  match e with
  | Const _ | Var _ -> 1
  | If_Zero (e1,e2,e3) -> 1 + expr_size e1 + expr_size e2 + expr_size e3
  | Unop (_,e) -> 1 + expr_size e
  | Binop (_,e1,e2) -> 1 + expr_size e1 + expr_size e2
  | Fold (e0,e1,_,_,e2) -> 2 + expr_size e2 + expr_size e2 + expr_size e0

let size p = 1 + expr_size p.expr

let single_hex_num c =
  match Int64.to_int c with
  | 0 ->  '0'
  | 1 ->  '1'
  | 2 ->  '2'
  | 3 ->  '3'
  | 4 ->  '4'
  | 5 ->  '5'
  | 6 ->  '6'
  | 7 ->  '7'
  | 8 ->  '8'
  | 9 ->  '9'
  | 10 -> 'A'
  | 11 -> 'B'
  | 12 -> 'C'
  | 13 -> 'D'
  | 14 -> 'E'
  | 15 -> 'F'
  | _ -> assert false
 
let int64_to_hex_string x =
  let s = String.copy "0x0000000000000000" in
  for i = 1 to 16 do
    s.[i+1] <-
      single_hex_num (Int64.shift_right x (4*(16-i)))
  done;
  s

let print_unop fmt op =
  match op with
  | Not -> Format.fprintf fmt "not"
  | Shl1 -> Format.fprintf fmt "shl1"
  | Shr1 -> Format.fprintf fmt "shr1"
  | Shr4 -> Format.fprintf fmt "shr4"
  | Shr16 -> Format.fprintf fmt "shr16"

let print_binop fmt op =
  match op with
  | And -> Format.fprintf fmt "and"
  | Or -> Format.fprintf fmt "or"
  | Xor -> Format.fprintf fmt "xor"
  | Plus -> Format.fprintf fmt "plus"

let print_var fmt x =
  print_id fmt x

let rec print_expr fmt e =
  match e with
  | Const c -> Format.fprintf fmt "%s" (int64_to_hex_string c)
  | Var x -> print_var fmt x
  | If_Zero (e1, e2, e3) ->
      Format.fprintf fmt "(if0 %a %a %a)"
      print_expr e1 print_expr e2 print_expr e3
  | Unop (op, e) ->
      Format.fprintf fmt "(%a %a)" print_unop op print_expr e
  | Binop (op, e1, e2) ->
      Format.fprintf fmt "(%a %a %a)" print_binop op print_expr e1 print_expr e2
  | Fold (e1,e2,x,y,e) ->
      Format.fprintf fmt "(fold %a %a (lambda (%a %a) %a))"
        print_expr e1 print_expr e2 print_var x print_var y print_expr e

let print_program fmt p =
  Format.fprintf fmt "(lambda (%a) %a)"
    print_var p.input
    print_expr p.expr
