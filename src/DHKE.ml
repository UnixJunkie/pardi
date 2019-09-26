
module DH = Cryptokit.DH

(* messages that will be exchanged during KE *)
type from_serv =
  | Serv_to_CLI_DHKE_params_bcast of Cryptokit.DH.parameters
  | Serv_to_CLI_DHKE_handshake of string
type from_cli =
  | CLI_to_Serv_DHKE_handshake of string

let signing_key_bit_len = 160
let signing_key_bytes_len = signing_key_bit_len / 8

let encryption_key_bit_len = 128
let encryption_key_bytes_len = encryption_key_bit_len / 8

let shared_secret_bit_len = signing_key_bit_len + encryption_key_bit_len
let shared_secret_bytes_len = shared_secret_bit_len / 8

(* parameters are public but new ones need to be created each time a DHKE
   is to take place; only called by the server once *)
let create_parameters () =
  (* 2048 is recommended by
     @inproceedings{weakdh15,
     title = {Imperfect Forward Secrecy: {H}ow {D}iffie-{H}ellman Fails
                  in Practice},
     author = {David Adrian and Karthikeyan Bhargavan and Zakir Durumeric
                  and Pierrick Gaudry and Matthew Green and J. Alex
                  Halderman and Nadia Heninger and Drew Springall and
                  Emmanuel Thom\'e and Luke Valenta and Benjamin
                  VanderSloot and Eric Wustrow and Santiago
                  Zanella-B\'eguelin and Paul Zimmermann},
     booktitle = {22nd ACM Conference on Computer and Communications
                  Security},
     month = oct,
     year = 2015
     } *)
  DH.new_parameters ~privlen:shared_secret_bit_len 2048

(* called by the server several times and one time by each client *)
let create_secret params =
  DH.private_secret params

let create_handshake_message_to_send params secret =
  DH.message params secret

let process_received_handshake_message params secret msg =
  (* Cryptokit wipes [secret] upon DH.shared_secret *)
  let shared_secret = DH.shared_secret params secret msg in
  (* derive keys from it *)
  let sign_encrypt_keys = DH.derive_key shared_secret shared_secret_bytes_len in
  Cryptokit.wipe_string shared_secret;
  assert((shared_secret_bytes_len =
          signing_key_bytes_len + encryption_key_bytes_len) &&
         signing_key_bit_len >= 160 && encryption_key_bit_len >= 128);
  (* cut into (signing_key, encryption_key) *)
  let signing_key =
    String.sub sign_encrypt_keys 0 signing_key_bytes_len in
  let encryption_key =
    String.sub sign_encrypt_keys signing_key_bytes_len encryption_key_bytes_len in
  Cryptokit.wipe_string sign_encrypt_keys;
  (signing_key, encryption_key)
