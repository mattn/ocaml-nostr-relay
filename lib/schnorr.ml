external verify_raw : string -> string -> string -> bool = "nostr_schnorr_verify"

(* [verify ~sig_hex ~pubkey_hex digest] checks a BIP-340 signature over the
   32 byte [digest] with libsecp256k1. *)
let verify ~sig_hex ~pubkey_hex digest =
  match (Util.of_hex sig_hex, Util.of_hex pubkey_hex) with
  | Some s, Some p
    when String.length s = 64 && String.length p = 32
         && String.length digest = 32 ->
      verify_raw s digest p
  | _ -> false
