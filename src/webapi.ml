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

let encode_buffer b =
  let s = Buffer.contents b in
  print_endline s;
  for i = 0 to String.length s - 1 do
    if s.[i] = ' ' then s.[i] <- '+'
  done;
  s

let connect request f =
  let addr = make_addr base_url 80 in
  run_client (fun sock ->
    let in_ch = in_channel_of_descr sock in
    let out_ch = out_channel_of_descr sock in
(*     let out_ch = open_out "toto" in *)
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
      let code = encode_buffer b in
      output_string out_ch code;
    end;
    output_string out_ch "\r\n";
    flush out_ch;
    let result = Buffer.create 257 in
    begin try while true do
      Buffer.add_string result (input_line in_ch);
      Buffer.add_char result '\n'
    done
    with End_of_file -> ()
    end;
    close sock;
    Buffer.contents result
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

let eval prog_id input =
  let x =
  connect "eval" (fun b ->
    Buffer.add_char b '{';
    Buffer.add_string b ("\"id\":\"" ^ prog_id ^ \",";
    Buffer.add_string b "\"arguments\":";
    print_as_json_dict b input;
    Buffer.add_string b "} ";) in
  print_endline x;
  [| |]

let my_problems () =
  let x = connect "myproblems" (fun b -> ()) in
  print_endline x;
  []
