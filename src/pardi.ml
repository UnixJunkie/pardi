
module CLI = Minicli.CLI
module Fn = Filename

open Printf

(* how to cut the input file into independant items *)
type demux_mode = Line (* default (SMI) *)
                | Sep of string (* (MOL2,SDF,PDB,etc.) *)
                | Bytes of int (* fixed block length *)
                | Reg of string (* a regexp separator *)

let read_some count csize input demux () =
  let read = ref 0 in
  let tmp_fn = Fn.temp_file "pardi_in_" ".txt" in
  Utls.with_out_file tmp_fn (fun out ->
      match demux with
      | Line ->
        (try
           for _ = 1 to csize do
             let line = input_line input in
             incr read;
             fprintf out "%s\n" line
           done
         with End_of_file -> ())
      | Sep _ -> failwith "Pardi.read_some: Sep: not implemented yet"
      | Bytes _ -> failwith "Pardi.read_some: Bytes: not implemented yet"
      | Reg _ -> failwith "Pardi.read_some: Reg: not implemented yet"
    );
  if !read = 0 then raise Parany.End_of_input;
  (* return item count and temp filename *)
  (* the count will be useful if the user wants to preserve input order *)
  let res = (!count, tmp_fn) in
  incr count;
  res

let input_fn_tag = Str.regexp "%INPUT"
let output_fn_tag = Str.regexp "%OUTPUT"

let process_some cmd (_count, tmp_in_fn) =
  (* FBR: preserve (and so count) is ignored for the moment *)
  assert(Str.string_match input_fn_tag cmd 0);
  let cmd' = Str.replace_first input_fn_tag tmp_in_fn cmd in
  let tmp_out_fn = Fn.temp_file "pardi_out_" ".txt" in
  assert(Str.string_match output_fn_tag cmd' 0);
  let cmd'' = Str.replace_first output_fn_tag tmp_out_fn cmd' in
  Utls.run_command cmd'';
  tmp_out_fn

type filename = string

type mux_mode = Cat of filename
              | Null

let gather_some mux_mode tmp_out_fn =
  match mux_mode with
  | Cat dst_fn ->
    let () = Utls.run_command (sprintf "cat %s >> %s" tmp_out_fn dst_fn) in
    Sys.remove tmp_out_fn
  | Null -> ()

let main () =
  Log.color_on ();
  Log.set_log_level Log.INFO;
  let argc, args = CLI.init () in
  let show_help = CLI.get_set_bool ["-h";"--help"] args in
  if argc = 1 || show_help then
    (eprintf "usage:\n\
              %s [...]\n\
              {-i|--input} <file>: where to read from (default: stdin)\n\
              {-o|--output} <file>: where to write to (default: stdout)\n\
              {-n|--nprocs} <int>: max jobs in parallel (default: all cores)\n\
              {-c|--chunks} <int>: how many chunks per job (default: 1)\n\
              {-d|--demux} {l|b:<int>|s:<string>}: \
              how to cut input file into chunks (default: l)\n\
              {-w|--work} <file>: script to execute on each chunk\n\
              {-m|--mux} {cat|null}: how to mux job results in output file \
                         (default: cat)\n\
              {-p|--preserve}: preserve input order (default: no)\n"
       Sys.argv.(0);
     exit 1);
  let in_chan = match CLI.get_string_opt ["-i";"--input"] args with
    | None -> stdin
    | Some fn -> open_in fn in
  let out_chan = match CLI.get_string_opt ["-o";"--output"] args with
    | None -> stdout
    | Some fn -> open_out fn in
  let _nprocs = match CLI.get_int_opt ["-n";"--nprocs"] args with
    | None -> Utls.get_nprocs ()
    | Some n -> n in
  let _csize = match CLI.get_int_opt ["-c";"--chunks"] args with
    | None -> 1
    | Some n -> n in
  (* FBR: TODO *)
  close_in in_chan;
  close_out out_chan

let () = main ()
