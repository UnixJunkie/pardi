
(* how to cut the input file into independant parts *)
type input_block = Line
                 | Bytes of int
                 | Sep of string
                 | Reg of string (* regexp *)

let main () =
  failwith "not implemented yet"

let () = main ()
