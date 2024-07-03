let usage_msg = "main [ARG] -c <count for child procesess>"
let input_files = ref []
let port = 54321
let count = ref 1
let anon_fun filename =
  input_files := filename :: !input_files

let speclist =
  [("-c", Arg.Set_int count, "Set count for child procesess")]


let () =
  Arg.parse speclist anon_fun usage_msg;
  (* Printf.printf "Child processes count: %d\n\r" !count;; *)

  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  let sock = Server.create_socket !count port in
  let serve = Server.create_server sock in
  Lwt_main.run @@ serve ()

