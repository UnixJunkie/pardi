
(* the following sizes are in bits *)
let signing_key_length = 160
let encryption_key_length = 128
let shared_secret_length = signing_key_length + encryption_key_length

(* parameters are public but new ones need to be created each time a DHKE
   is to take place *)
let create_parameters () =
  Cryptokit.new_parameters ~privlen:shared_secret_length 2048
