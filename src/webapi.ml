open Unix

let base_url = "icfpc2013.cloudapp.net"
let host_name = "toto"

let token =
  let fd = open_in "secret.txt" in
  let s = input_line fd in
  s ^ "vpsH1H"

let path = "/train?auth=" ^ token

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
let test () =
  let addr = make_addr base_url 80 in
  run_client (fun sock ->
    let in_ch = in_channel_of_descr sock in
    let out_ch = out_channel_of_descr sock in
    output_string out_ch ("GET "^path^" HTTP/1.1\r\n");
    output_string out_ch ("Host: "^host_name^"\r\n");
    output_string out_ch "User-Agent: mon programme\r\n";
    output_string out_ch "Connection: close\r\n";
    output_string out_ch "\r\n";
    output_string out_ch "";
    flush out_ch;
    begin try while true do
       print_endline (input_line in_ch)
    done
    with End_of_file -> ()
    end;
    close sock
  ) addr
