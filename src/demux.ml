
(* how to cut the input file into independant items *)

type t = Line (* default (e.g. for SMILES files) *)
       | Sep of string (* (MOL2, SDF, PDB, etc.) *)
       | Bytes of int (* fixed block size *)
       | Reg of string (* a line-matching regexp separator *)

let of_string = function
  | "l" -> Line
  | demux_opt ->
    let head, tail = BatString.split demux_opt ~by":" in
    match head with
    | "b" -> Bytes (int_of_string tail)
    | "s" -> Sep tail
    | "r" -> Reg tail
    | _ -> failwith ("Demux.of_string: unknown demux mode: " ^ tail)
