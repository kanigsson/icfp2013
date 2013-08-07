open Unix

type problem_id = string

let base_url = "icfpc2013.cloudapp.net"
let host_name = "my host"

let token =
  let fd = open_in "secret.txt" in
  let s = input_line fd in
  s ^ "vpsH1H"

let path request = "/" ^ request ^ "?auth=" ^ token

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

let connect request f =
  let addr = make_addr base_url 80 in
  run_client (fun sock ->
    let in_ch = in_channel_of_descr sock in
    let out_ch = out_channel_of_descr sock in
    output_string out_ch ("POST "^path request^" HTTP/1.1\r\n");
    output_string out_ch ("Host: "^host_name^"\r\n");
    output_string out_ch "User-Agent: dummy\r\n";
    output_string out_ch "Connection: close\r\n";
    output_string out_ch "Content_Type: application/json\r\n";
    output_string out_ch "\r\n";
    output_string out_ch "";
    f out_ch;
    flush out_ch;
    let result = ref "" in
    begin try while true do
      result := !result ^ (input_line in_ch);
    done
    with End_of_file -> ()
    end;
    close sock;
    !result
  ) addr

let num_to_hex_string x = assert false

let print_as_json_dict ch ar =
  output_string ch "[";
  for i = 0 to Array.length ar -1 do
    if i <> 0 then output_string ch ",";
    output_string ch "\"";
    output_string ch (num_to_hex_string ar.(i));
    output_string ch "\"\r\n";
  done;
  output_string ch "]"

let eval prog_id input =
  let x =
  connect "eval" (fun ch ->
    output_string ch "{";
    output_string ch "program: \"(lambda (x_38661) (not x_38661))\"";
    output_string ch "arguments: ";
    print_as_json_dict ch input;
    output_string ch "} ";) in
  print_endline x;
  [| |]
