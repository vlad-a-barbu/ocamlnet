let send to_addr n =
  let open Unix in
  let fd = socket PF_INET SOCK_STREAM 0 in
  connect fd to_addr;
  let payload = Bytes.init n @@ fun _ -> 'x' in
  send fd payload 0 n []

let () =
  let sent = send (Unix.ADDR_INET (Unix.inet_addr_loopback, 3000)) 1_000_000 in
  Printf.printf "\nsent %d bytes\n%!" sent
