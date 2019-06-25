
module CLI = Minicli.CLI
module Fn = Filename

open Printf

let rec read_one_block
    (buff: Buffer.t)
    (block_end: string -> bool)
    (input: in_channel): string =
  try
    let line = input_line input in
    if block_end line && Buffer.length buff > 0 then
      (* the previous block ends just before this line *)
      let res = Buffer.contents buff in
      Buffer.clear buff;
      Buffer.add_string buff line;
      Buffer.add_char buff '\n'; (* add back line terminator *)
      res
    else
      begin (* add to block under construction *)
        Buffer.add_string buff line;
        Buffer.add_char buff '\n'; (* add back line terminator *)
        read_one_block buff block_end input
      end
  with End_of_file ->
    if Buffer.length buff > 0 then
      Buffer.contents buff (* last block read *)
    else
      raise End_of_file (* no more blocks *)

let read_some count csize input demux () =
  let read = ref 0 in
  let tmp_fn = Fn.temp_file "pardi_in_" ".txt" in
  Utls.with_out_file tmp_fn (fun out ->
      match demux with
      | Demux.Line ->
        (try
           for _ = 1 to csize do
             let line = input_line input in
             incr read;
             fprintf out "%s\n" line
           done
         with End_of_file -> ())
      | Demux.Sep _ -> failwith "Pardi.read_some: Sep: not implemented yet"
      | Demux.Bytes _ -> failwith "Pardi.read_some: Bytes: not implemented yet"
      | Demux.Reg reg ->
        let stop_cond line =
          Str.string_match reg line 0 in
        let buff = Buffer.create 1024 in
        try
          for _ = 1 to csize do
            let block = read_one_block buff stop_cond input in
            incr read;
            fprintf out "%s" block
          done
        with End_of_file -> ()
    );
  if !read = 0 then raise Parany.End_of_input;
  (* return item count and temp filename *)
  (* the count will be useful if the user wants to preserve input order *)
  let res = (!count, tmp_fn) in
  incr count;
  res

let input_fn_tag = Str.regexp "%IN"
let output_fn_tag = Str.regexp "%OUT"

let process_some debug cmd (_count, tmp_in_fn) =
  (* FBR: --preserve (and so count) is ignored for the moment *)
  assert(Utls.regexp_in_string input_fn_tag cmd);
  let cmd' = Str.replace_first input_fn_tag tmp_in_fn cmd in
  let tmp_out_fn = Fn.temp_file "pardi_out_" ".txt" in
  assert(Utls.regexp_in_string output_fn_tag cmd');
  let cmd'' = Str.replace_first output_fn_tag tmp_out_fn cmd' in
  Utls.run_command debug cmd'';
  tmp_out_fn

type filename = string

type mux_mode = Cat of filename
              | Null

let mux_count = ref 0

let gather_some debug mux_mode tmp_out_fn =
  match mux_mode with
  | Null -> ()
  | Cat dst_fn ->
    begin
      let cmd =
        sprintf (if !mux_count = 0
                 then "cp -f %s %s" (* crush existing file, if any *)
                 else "cat %s >> %s" (* or just append to it *)
                ) tmp_out_fn dst_fn in
      Utls.run_command debug cmd;
      Sys.remove tmp_out_fn;
      incr mux_count;
      printf "processed: %d\r%!" !mux_count (* user feedback *)
    end

let main () =
  Log.color_on ();
  Log.set_log_level Log.INFO;
  let argc, args = CLI.init () in
  let show_help = CLI.get_set_bool ["-h";"--help"] args in
  if argc = 1 || show_help then
    (eprintf "usage:\n\
              %s ...\n\
              {-i|--input} <file>: where to read from (default: stdin)\n\
              {-o|--output} <file>: where to write to (default: stdout)\n\
              {-n|--nprocs} <int>: max jobs in parallel (default: all cores)\n\
              {-c|--chunks} <int>: how many chunks per job (default: 1)\n\
              {-d|--demux} {l|b:<int>|s:<string>}: \
              how to cut input file into chunks (default: l)\n\
              {-w|--work} <string>: command to execute on each chunk\n\
              {-m|--mux} {cat|null}: how to mux job results in output file \
                         (default: cat)\n\
              {-p|--preserve}: preserve input order (default: no)\n"
       Sys.argv.(0);
     exit 1);
  let debug = CLI.get_set_bool ["-v";"--verbose"] args in
  let in_chan = match CLI.get_string_opt ["-i";"--input"] args with
    | None -> stdin
    | Some fn -> open_in fn in
  let out_fn = CLI.get_string_def ["-o";"--output"] args "/dev/stdout" in
  let cmd = CLI.get_string ["-w";"--work"] args in
  let nprocs = match CLI.get_int_opt ["-n";"--nprocs"] args with
    | None -> Utls.get_nprocs ()
    | Some n -> n in
  let csize = match CLI.get_int_opt ["-c";"--chunks"] args with
    | None -> 1
    | Some n -> n in
  let demux =
    let demux_str = CLI.get_string_def ["-d";"--demux"] args "l" in
    Demux.of_string demux_str in
  CLI.finalize ();
  let count = ref 0 in
  (* Parany has a csize of one, because read_some takes care of the number
     of chunks per job *)
  Parany.run ~verbose:false ~csize:1 ~nprocs
    ~demux:(read_some count csize in_chan demux)
    ~work:(process_some debug cmd)
    ~mux:(gather_some debug (Cat out_fn));
  printf "\n%!";
  close_in in_chan

let () = main ()
