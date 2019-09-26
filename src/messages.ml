
type from_serv =
  | Serv_to_CLI_DHKE_params_bcast of Cryptokit.DH.parameters
  | Serv_to_CLI_DHKE_handshake of string

type from_cli =
  | CLI_to_Serv_DHKE_handshake of string
