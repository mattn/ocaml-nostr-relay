open Lwt.Infix
open Nostr_relay

let port =
  match int_of_string_opt (Util.getenv ~default:"9001" "PORT") with Some p -> p | None -> 9001

(* Prefer the address the reverse proxy (Cloudflare Tunnel, ingress) saw. *)
let client_ip request =
  let from_header name =
    match Dream.header request name with
    | Some value when String.trim value <> "" ->
        Some (String.trim (List.hd (String.split_on_char ',' value)))
    | _ -> None
  in
  match List.find_map from_header [ "cf-connecting-ip"; "x-forwarded-for"; "x-real-ip" ] with
  | Some ip -> ip
  | None -> Dream.client request

let relay_url request =
  match Sys.getenv_opt "RELAY_URL" with
  | Some url when url <> "" -> url
  | _ -> ( match Dream.header request "host" with Some host -> "wss://" ^ host | None -> "")

let contains ~needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec loop i = i + n <= h && (String.sub haystack i n = needle || loop (i + 1)) in
  n = 0 || loop 0

let websocket request =
  Dream.websocket (fun ws ->
      let conn = Relay.create ~ws ~ip:(client_ip request) ~relay_url:(relay_url request) in
      Dream.log "[%s] client connected" conn.Relay.ip;
      let rec loop () =
        Dream.receive ws >>= function
        | None -> Lwt.return_unit
        | Some message ->
            Dream.log "[%s] %s" conn.Relay.ip message;
            Relay.handle conn message >>= loop
      in
      Lwt.finalize
        (fun () ->
          Lwt.catch (fun () -> Relay.auth_challenge conn >>= loop) (fun _ -> Lwt.return_unit))
        (fun () ->
          Relay.close conn;
          Dream.log "[%s] client disconnected" conn.Relay.ip;
          Lwt.return_unit))

let root request =
  let header name = Option.value ~default:"" (Dream.header request name) in
  if String.lowercase_ascii (header "upgrade") = "websocket" then websocket request
  else if contains ~needle:"application/nostr+json" (header "accept") then
    Dream.respond
      ~headers:
        [
          ("Content-Type", "application/nostr+json"); ("Access-Control-Allow-Origin", "*");
        ]
      (Yojson.Safe.to_string (Nip11.document ()))
  else Dream.from_filesystem "public" "index.html" request

let () =
  Lwt_main.run
    (Lwt.catch Store.init (fun exn ->
         prerr_endline ("failed to initialize schema: " ^ Printexc.to_string exn);
         Lwt.return_unit));
  Dream.run ~interface:"0.0.0.0" ~port @@ Dream.logger
  @@ Dream.router [ Dream.get "/" root; Dream.get "/**" (Dream.static "public") ]
