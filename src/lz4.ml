
(* LZ4-encoded data; type needed because LZ4 doesn't store the length of the
   data if uncompressed... *)
type t = { orig_length: int; (* length if decompressed *)
           compressed: Bytes.t }

let compress (s: string): t =
  let orig_length = String.length s in
  let compressed = LZ4.Bytes.compress (Bytes.unsafe_of_string s) in
  { orig_length; compressed }

let decompress (c: t): string =
  let buff = LZ4.Bytes.decompress ~length:c.orig_length c.compressed in
  Bytes.unsafe_to_string buff
