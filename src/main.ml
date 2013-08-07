open Programs

let _ =
  print_endline "hello";
  let x = Webapi.eval "1" (Array.map Int64.of_int [|12;13|]) in
  for i = 0 to Array.length x - 1 do
    print_endline (Int64.to_string x.(i))
  done

