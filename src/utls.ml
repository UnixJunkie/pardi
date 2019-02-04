
let get_nprocs () =
  let _stat, out_str = BatUnix.run_and_read "getconf _NPROCESSORS_ONLN" in
  Scanf.sscanf out_str "%d" (fun x -> x)
