
(* LZ4-encoded data *)
type t = { orig_length: int; (* length if decompressed *)
           compressed: Bytes.t }

let compress (s: string): t =
  let orig_length = String.length s in
  let compressed = LZ4.Bytes.compress (Bytes.unsafe_of_string s) in
  { orig_length; compressed }

let decompress (c: t): string =
  let buff = Bytes.create c.orig_length in
  LZ4.Bytes.decompress ~length:c.orig_length c.compressed;
  Bytes.unsafe_to_string buff
