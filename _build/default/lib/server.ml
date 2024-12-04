type config = { port : int; max_pending_requests : int; }

let default_config = { port = 3000; max_pending_requests = 333; }

let setup config =
  let open Unix in
  let fd = socket PF_INET SOCK_STREAM 0 in
  setsockopt fd SO_REUSEADDR true;
  bind fd @@ ADDR_INET (inet_addr_loopback, config.port);
  listen fd config.max_pending_requests;
  fd

let handle_client_conn (fd, _addr) =
  let cnt = ref 0 in
  let rec recv_ic ic =
    try
      let _ = input_byte ic in
      cnt := !cnt + 1;
      recv_ic ic;
    with
      _ -> ()
  in
  let open Unix in
  recv_ic @@ in_channel_of_descr fd;
  let rec log = function
    | [] -> ()
    | hd :: tl ->
      Printf.fprintf (out_channel_of_descr hd) "\nreceived %d bytes\n%!" !cnt;
      log tl
  in
  log [stdout ; fd];
  close fd

let rec accept_loop fd =
  let open Unix in
  handle_client_conn @@ accept fd;
  accept_loop fd

let start ?(config=default_config) () =
  let fd = setup config in
  Printf.printf "\nserver listening on port %d\n%!" config.port;
  accept_loop fd

(* head -c 100000000 /dev/urandom | nc localhost 3000 *)
(* received 22380535 bytes ??? *)
