open Format

let ksprintf k s =
  ignore(flush_str_formatter ());
  kfprintf (fun _ -> k (flush_str_formatter ())) str_formatter s

let sprintf s = ksprintf (fun x -> x) s


