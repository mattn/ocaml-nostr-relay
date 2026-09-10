let stamp () =
  let now = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%02d.%02d.%02d %02d:%02d:%02d"
    (now.Unix.tm_year mod 100) (now.Unix.tm_mon + 1) now.Unix.tm_mday now.Unix.tm_hour
    now.Unix.tm_min now.Unix.tm_sec

let info fmt =
  Printf.ksprintf
    (fun message ->
      print_string (stamp () ^ " " ^ message ^ "\n");
      flush stdout)
    fmt

let error fmt =
  Printf.ksprintf
    (fun message ->
      prerr_string (stamp () ^ " [error] " ^ message ^ "\n");
      flush stderr)
    fmt
