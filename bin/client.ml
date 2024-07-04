open Lwt.Infix
open Lwt_unix

let port = 54321
let rec read_input_and_send fd  =
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
  Lwt_io.read_line ic >>= fun message ->
  let len_msg = string_of_int (String.length message) in
  Lwt_io.write_line oc len_msg >>= fun () ->
  Lwt_io.printf "Some action.\n" >>= fun () ->
  read_input_and_send fd 

let connect_to_server port () =
  let server_address = ADDR_INET (Unix.inet_addr_loopback, port) in
  let socket = socket PF_INET SOCK_STREAM 0 in
  Lwt_unix.connect socket server_address >>= fun () ->
  Lwt_io.printf "Connected to the server.\n" >>= fun () ->
  read_input_and_send socket

let () =
  let client = connect_to_server port () in
  Lwt_main.run client

  (* ocamlfind ocamlopt -o client -linkpkg -thread -package lwt.unix bin/client.ml *)