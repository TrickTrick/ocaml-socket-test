open Lwt.Infix
open Lwt_unix

let min_con = 60
let max_con = 120
let min_keep = 5
let max_keep = 20
let rec random_int_in_range min max =
  let v = Random.int max + 1 in
  match v with
  | v when v >= min -> v
  | _ -> random_int_in_range min max

let rec read_input_and_send fd  =
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  Lwt_io.read_line_opt ic >>=
    (fun msg ->
        match msg with
        | Some msg -> 
          Lwt_io.printf "From Server message: %s\n" msg  >>= fun () ->
          let len_msg = string_of_int (String.length msg) in
          Lwt_io.write_line oc len_msg >>= fun () ->
          read_input_and_send fd
        | None ->
          Lwt.return_unit)

let shutdown_process socket () = 
  Lwt_unix.shutdown socket Unix.SHUTDOWN_ALL;
  Lwt.return_unit

let shout_keep fd =
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  Lwt_io.write_line oc "keep-alive"

(* Function to create a timeout for a callback *)
let set_timeout duration callback =
  Lwt_unix.sleep duration >>= fun () ->
  callback ()

let rec set_interval i callback =
  Lwt_unix.sleep i >>= fun () ->
  callback () >>= fun () ->
  set_interval i callback

let connect_to_server addr port () =
  let server_address = ADDR_INET (addr, port) in
  let socket = socket PF_INET SOCK_STREAM 0 in

  (* random disconnect *)
  let disc_duration = float_of_int (random_int_in_range min_con max_con) in
  let disc_timeout = set_timeout disc_duration (shutdown_process socket) in
  Lwt.async (fun () -> disc_timeout);

  (* keep alive msg *)
  let keep_duration = float_of_int (random_int_in_range min_keep max_keep) in
  let cb () = shout_keep socket in 
  let keep_timeout = set_interval keep_duration cb in
  Lwt.async (fun () -> keep_timeout);

  Lwt_unix.connect socket server_address >>= fun () ->
  Lwt_io.printf "Connected to the server.\n" >>= fun () ->
  read_input_and_send socket

let () =
  let client = connect_to_server Unix.inet_addr_loopback 54321 () in
  Lwt_main.run client

(* ocamlfind ocamlopt -o client -linkpkg -thread -package lwt.unix bin/client.ml *)