open Lwt
open Lwt.Infix

let usage_msg = "main [ARG] -c <count for child procesess>"
let input_files = ref []
let anon_fun filename =
  input_files := filename :: !input_files

let count = ref 1
let addr = Unix.inet_addr_loopback
let port = 54321
let speclist =
  [("-c", Arg.Set_int count, "Set count for child procesess")]

let rec run_command cmd args =
  let process = Lwt_process.open_process_in (cmd, Array.of_list (cmd :: args)) in
  Lwt_io.read process#stdout >>= fun _ ->
  process#close >>= fun _ -> 
  run_command cmd args (* auto start after die *)
  (* @todo log it *)

let () =
  Arg.parse speclist anon_fun usage_msg;

  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in

  Server.counter := !count;
  let sock = Server.create_socket addr port in
  let serve = Server.create_server sock in

  let start_processes = 
    let cmd = "./client" in
    let args = [] in
    Lwt_list.iter_p (fun _ -> run_command cmd args) (List.init !count (fun x -> x))
  in

  let clients =
    Lwt_unix.sleep 1. >>= fun () -> 
    start_processes
  in

  let server = serve () in
  let stdin_watcher = Server.watch_stdin () in
  let print_clients_length = Server.print_clients_length () in
  (* let logger = setup_logging () in *)

  Lwt_main.run (Lwt.join [server; clients; stdin_watcher; print_clients_length])


