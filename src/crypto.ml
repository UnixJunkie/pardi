
(* create fresh nonces and check their freshness *)
(* FBR: move to Ht *)
module Nonce_store = struct
  let nonces = ref StringSet.empty
  let fresh (counter: int ref) (n: Node.t): string =
    let nonce =
      sprintf "%s:%d:%d" (Node.get_host n) (Node.get_port n) !counter
    in
    if !counter = -1 then failwith "fresh: counter has looped";
    incr counter;
    nonces := StringSet.add nonce !nonces;
    nonce
  let is_fresh (nonce: string): bool =
    if StringSet.mem nonce !nonces then
      false
    else begin
      nonces := StringSet.add nonce !nonces;
      true
    end
end

  (* FBR: needs a Flags module, with use_compression default to true
   * toggable to off by CLI options *)

(* FBR: use LZ4 *)
let compress msg =
  failwith "not implemented yet"

let uncompress msg =
  failwith "not implemented yet"

(* signature of the message (length 20B = 160bits) *)
let sign (msg: string): string =
  let signer = Cryptokit.MAC.hmac_ripemd160 (get_key `Sign) in
  signer#add_string msg;
  let signature = signer#result in
  signer#wipe;
  assert(String.length signature = 20);
  signature

(* optionally return the message without its prefix signature or None
   if the signature is incorrect or anything strange was found *)
let check_sign (s: string option): string option =
  match s with
  | None -> None
  | Some msg ->
    let n = String.length msg in
    if n <= 21 then (* 20B signature plus at least 1B message *)
      Utils.ignore_first (Log.error "check_sign: message too short: %d" n) None
    else
      let prev_sign = String.sub msg 0 20 in
      let signless = String.sub msg 20 (n - 20) in
      let curr_sign = sign signless in
      if curr_sign <> prev_sign then
        Utils.ignore_first (Log.error "check_sign: bad signature") None
      else
        Some signless

(* FBR: move to chacha20 *)
let encrypt (msg: string): string =
  let enigma =
    new Cryptokit.Block.cipher_padded_encrypt Cryptokit.Padding.length
      (new Cryptokit.Block.cbc_encrypt
        (new Cryptokit.Block.blowfish_encrypt (get_key `Cipher)))
  in
  enigma#put_string msg;
  enigma#finish;
  let res = enigma#get_string in
  enigma#wipe;
  res

(* FBR: move to chacha20 *)
let decrypt (msg: string): string option =
  let turing =
    new Cryptokit.Block.cipher_padded_decrypt Cryptokit.Padding.length
      (new Cryptokit.Block.cbc_decrypt
        (new Cryptokit.Block.blowfish_decrypt (get_key `Cipher)))
  in
  turing#put_string msg;
  turing#finish;
  let res = turing#get_string in
  turing#wipe;
  Some res

(* FBR: move to encrypt then sign *)
(* m --> encrypt (signature ^ salt ^ nonce ^ compress(marshall m)) *)
let encode
    (rng: Cryptokit.Random.rng)
    (counter: int ref)
    (sender: Node.t)
    (m: 'a): string =
  let plain_text = Marshal.to_string m [Marshal.No_sharing] in
  let maybe_compressed = compress plain_text in
  let salt = String.make 8 '0' in (* 64 bits salt *)
  rng#random_bytes salt 0 8;
  (* let salt_hex = Utils.convert `To_hexa salt in *)
  (* Log.debug "enc. salt = %s" salt_hex; *)
  let nonce = Nonce_store.fresh counter sender in
  (* Log.debug "enc. nonce = %s" nonce; *)
  let s_n_m = salt ^ nonce ^ "|" ^ maybe_compressed in
  let signed = (sign s_n_m) ^ s_n_m in
  encrypt signed

(* FBR: move to check_sign then decrypt *)
let decode (s: string): 'a option =
  match check_sign (decrypt s) with
  | None -> None
  | Some str ->
    (* ignore salt (8 first bytes) *)
      (*
        let salt = String.sub str 0 8 in
        let salt_hex = Utils.convert `To_hexa salt in
        Log.debug "dec. salt = %s" salt_hex;
      *)
    let n = String.length str in
    let nonce_end = String.index_from str 8 '|' in
    assert(nonce_end > 8 && nonce_end < n);
    let nonce = String.sub str 8 (nonce_end - 8) in
    (* Log.debug "dec. nonce = %s" nonce; *)
    if Nonce_store.is_fresh nonce then
      let maybe_compressed = BatString.lchop ~n:(nonce_end + 1) str in
      match uncompress maybe_compressed with
      | None -> None
      | Some u -> Some (Marshal.from_string u 0: 'a)
    else
      Utils.ignore_first (Log.warn "nonce already seen: %s" nonce) None
