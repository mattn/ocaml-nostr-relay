(* NIP-11: Relay Information Document *)

let version = "0.0.2"
let supported_nips = [ 1; 4; 9; 11; 17; 26; 40; 42; 45; 50; 59; 66; 70; 78 ]

let countries () =
  Util.getenv ~default:"JP" "RELAY_COUNTRIES"
  |> String.split_on_char ',' |> List.map String.trim
  |> List.filter (fun country -> country <> "")

let document () : Yojson.Safe.t =
  `Assoc
    [
      ("name", `String (Util.getenv ~default:"ocaml-nostr-relay" "RELAY_NAME"));
      ( "description",
        `String (Util.getenv ~default:"A Nostr relay written in OCaml" "RELAY_DESCRIPTION") );
      ("pubkey", `String (Util.getenv "RELAY_PUBKEY"));
      ("contact", `String (Util.getenv "RELAY_CONTACT"));
      ("icon", `String (Util.getenv "RELAY_ICON"));
      ("supported_nips", `List (List.map (fun nip -> `Int nip) supported_nips));
      ("software", `String "https://github.com/mattn/ocaml-nostr-relay");
      ("version", `String version);
      ("relay_countries", `List (List.map (fun c -> `String c) (countries ())));
    ]
