open Unix

type problem_id = string

type problem =
  { id : problem_id;
    size : int;
    operators : string;
  }

let base_url = "icfpc2013.cloudapp.net"
let host_name = "myhost"

let token =
  let fd = open_in "secret.txt" in
  let s = input_line fd in
  s ^ "vpsH1H"

let auth_string = "auth=" ^ token
let get_path post_get request = post_get ^ request ^ "?" ^ auth_string

let make_addr host_name port =
  let host_addr =
    try (gethostbyname host_name).h_addr_list.(0)
    with Not_found -> 
      prerr_endline (host_name ^ ": Host not found");
      exit 2
  in
  ADDR_INET (host_addr, port)

let run_client f addr =
  let sock = socket PF_INET SOCK_STREAM 0 in
  connect sock addr;
  f sock

type regexp = 
    { regexp : Str.regexp; 
      fields : (int * string option) list; }

let regexp_match r string =
  let get (pos, default) =
    try Str.matched_group pos string
    with Not_found ->
      begin match default with 
      | Some s -> s
      | _ -> raise Not_found 
      end
  in
  try
    if Str.string_match r.regexp string 0 then
      Some (List.map get r.fields)
    else None
  with Not_found -> None
   
let response_regexp = 
  { regexp = Str.regexp "^HTTP/1.[01][ \t]+\\([0-9]+\\)[ \t]+\\(.*[^\r]\\)\r";
    fields = [ 1, None; 2, None; ] }

exception Error of string

let error err mes = raise (Error (err ^ ": " ^ mes))

let parse_response line =
  match regexp_match response_regexp line with
  | Some (code :: msg :: _) -> (int_of_string code, msg)
  | _ -> error line "Ill formed response"

let rec remove_headers in_ch =
  match input_line in_ch with
  | "\r" -> ()
  | line -> remove_headers in_ch 

type result =
  | OK of string
  | Error of string

let debug_print_result r =
  match r with
  | OK s -> Format.printf "%s@." s
  | Error s -> Format.printf "Error %s@." s

let connect request f =
  let addr = make_addr base_url 80 in
  run_client (fun sock ->
    let in_ch = in_channel_of_descr sock in
    let out_ch = out_channel_of_descr sock in
    output_string out_ch (get_path "POST /" request ^ " HTTP/1.1\r\n");
    output_string out_ch ("Host: "^host_name^"\r\n");
    output_string out_ch "User-Agent: dummy\r\n";
    output_string out_ch "Connection: close\r\n";
    let b = Buffer.create 257 in
    f b;
    if Buffer.length b > 0 then begin
      output_string out_ch "Content-Type: application/json\r\n";
      output_string out_ch "Content-Length: ";
      output_string out_ch (string_of_int (Buffer.length b + 4));
      output_string out_ch "\r\n";
      output_string out_ch "\r\n";
      output_string out_ch (Buffer.contents b);
    end;
    output_string out_ch "\r\n";
    output_string out_ch "\r\n";
    flush out_ch;
    let s = 
      try
        match parse_response (input_line in_ch) with
        | 200, _ ->
            remove_headers in_ch;
            let _ = input_line in_ch in
            let dict = input_line in_ch in
            begin try while true do
              ignore (input_line in_ch)
            done with End_of_file -> () end;
            OK dict
        | _, s -> Error s 
      with End_of_file -> Error "unexpected end of stream"
    in
    close sock;
    s
  ) addr

let print_as_json_dict b ar =
  Buffer.add_char b '[';
  for i = 0 to Array.length ar -1 do
    if i <> 0 then Buffer.add_char b ',';
    Buffer.add_char b '"';
    Buffer.add_string b (Programs.int64_to_hex_string ar.(i));
    Buffer.add_char b '"';
  done;
  Buffer.add_char b ']'

let handle_server_error x =
  match x with
  | OK s -> s
  | Error s -> 
      Format.printf "server error: %s" s;
      assert false

let eval prog_id input =
  let x =
  connect "eval" (fun b ->
    Buffer.add_char b '{';
    Buffer.add_string b ("\"id\":\"" ^ prog_id ^ "\",");
    Buffer.add_string b "\"arguments\":";
    print_as_json_dict b input;
    Buffer.add_string b "} ";) in
  let s = handle_server_error x in
  let r = Json.eval_response_of_string s in
  match r with
  | Json.Eval_ok a -> a
  | Json.Eval_error s ->
      Format.printf "Error in answer: %s@." s;
      assert false

let guess prog_id prog =
  let x =
  connect "guess" (fun b ->
    Buffer.add_char b '{';
    Buffer.add_string b ("\"id\":\"" ^ prog_id ^ "\",");
    Buffer.add_string b "\"program\":";
    Buffer.add_char b '"';
    Buffer.add_string b (Util.sprintf "%a" Programs.print_program prog);
    Buffer.add_char b '"';
    Buffer.add_string b "} ";) in
  let s = handle_server_error x in
  let r = Json.guess_response_of_string s in
  r

let my_problems () =
  let x = connect "myproblems" (fun b -> ()) in
  debug_print_result x;
  []

let train size =
  let x =
  connect "train" (fun b ->
    Buffer.add_string b "{\"size\" :";
    Buffer.add_string b (string_of_int size);
    Buffer.add_string b "}";
    Buffer.add_char b) in
  debug_print_result x
