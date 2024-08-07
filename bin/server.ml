open Lwt

let counter = ref 0

let clients = ref []

let broadcast_message message =
    Lwt_list.iter_p (fun oc ->
        Lwt_io.write_line oc message 
        >>= fun () -> Lwt_io.printf "[Server] Message oc: %s\n" message 
        >>= fun () -> Lwt_io.flush oc
    ) !clients

let watch_stdin () =
    let rec read_stdin () =
        Lwt_io.read_line Lwt_io.stdin >>= fun command ->
        broadcast_message command >>= read_stdin
    in
    read_stdin ()

let check_all_clients_created cnt = List.length !clients >= cnt
  
let print_clients_length () = 
    let created = check_all_clients_created !counter in
    Lwt_io.printl (
        "All clients created: " 
        ^ (string_of_bool created) 
        ^ "\nArgs cnt: "
        ^ (string_of_int !counter)
        ^ "\nClients cnt: " 
        ^ (string_of_int (List.length !clients))
    )

let rec handle_connection ic oc () =
    Lwt_io.read_line_opt ic >>=
    (fun msg ->
        match msg with
        | Some msg -> 
            Lwt_io.printf "From client %s.\n" msg >>=
            handle_connection ic oc
        | None ->
            Lwt_io.printf "Client disconnected.\n" >>= fun () ->
            clients := List.filter (fun c -> c != oc) !clients;
            Lwt.return_unit)

let accept_connection conn =
    let fd, _ = conn in
    let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
    clients := oc :: !clients;
    (* debug  *)
    let _ = print_clients_length () in 
    Lwt.on_failure (handle_connection ic oc ()) (fun e -> Logs.err (fun m -> m "%s" (Printexc.to_string e) ));
    Logs_lwt.info (fun m -> m "New connection") >>=
    return

let create_socket addr port =
    let open Lwt_unix in
    let sock = socket PF_INET SOCK_STREAM 0 in
    bind sock @@ ADDR_INET(addr, port) |> (fun x -> ignore x);
    listen sock !counter;
    sock

let create_server sock =
    let rec serve () =
        Lwt_unix.accept sock >>= accept_connection >>= serve
    in serve
