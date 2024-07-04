let usage_msg = "main [ARG] -c <count for child procesess>"
let input_files = ref []
let anon_fun filename =
  input_files := filename :: !input_files

let count = ref 1
let addr = Unix.inet_addr_loopback
let port = 54321
let speclist =
  [("-c", Arg.Set_int count, "Set count for child procesess")]

let () =
  Arg.parse speclist anon_fun usage_msg;

  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  Server.counter := !count;
  let sock = Server.create_socket addr port in
  let serve = Server.create_server sock in
  let server = serve () in
  let stdin_watcher = Server.watch_stdin () in
  let print_clients_length = Server.print_clients_length () in

  (* let client = Client.connect_to_server addr port in *)
  Lwt_main.run (Lwt.join [server; stdin_watcher; print_clients_length])
  (* Lwt_main.run @@ serve () *)

