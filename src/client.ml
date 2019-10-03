
open Printf

module CLI = Minicli.CLI
module Log = Dolog.Log

let main () =
  Log.color_on ();
  Log.set_log_level Log.INFO;
  let argc, args = CLI.init () in
  let show_help = CLI.get_set_bool ["-h";"--help"] args in
  if argc = 1 || show_help then
    (eprintf "usage:\n\
              %s ...\n  \
              {-s|--server} <string>: server hostname\n  \
              {-p|--port} <int>: server port\n  \
              [{-n|--nprocs} <int>]: max jobs in parallel \
              (default=all cores)\n  \
              [{-c|--chunks} <int>]: how many jobs per request (default=1)\n"
       Sys.argv.(0);
     exit 1);
  Flags.debug := CLI.get_set_bool ["-v";"--verbose"] args;
  let _server_name = CLI.get_string ["-s";"--server"] args in
  let _server_port = CLI.get_int ["-p";"--port"] args in
  let _nprocs = match CLI.get_int_opt ["-n";"--nprocs"] args with
    | None -> Utls.get_nprocs ()
    | Some n -> n in
  let _jobs_per_request = match CLI.get_int_opt ["-c";"--chunks"] args with
    | None -> 1
    | Some n -> n in
  CLI.finalize ();
  Log.info "contacting server...";
  let work_dir = Utls.get_command_output !Flags.debug "mktemp -d -t pardi_XXXX" in
  Log.info "work_dir: %s" work_dir;
  Parany.set_copy_on_work ();
  Parany.set_copy_on_mux ();
  let _ctx = Zmq.Context.create () in
  (* Parany.run ~verbose:false ~csize:1 ~nprocs
   *   ~demux:(read_some work_dir input_ext
   *             (Buffer.create 1024) (ref 0) csize in_chan demux)
   *   ~work:(process_some output_ext cmd)
   *   ~mux:(gather_some total_items start_t (ref 0) mux); *)
  if not !Flags.debug then
    Utls.run_command !Flags.debug (sprintf "rm -rf %s" work_dir)

let () = main ()
