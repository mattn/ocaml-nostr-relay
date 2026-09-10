open Lwt.Infix
open Nostr_relay
module Frame = Websocket.Frame

let port =
  match int_of_string_opt (Util.getenv ~default:"9001" "PORT") with Some p -> p | None -> 9001

let header request name = Cohttp.Header.get (Cohttp.Request.headers request) name

(* Prefer the address the reverse proxy (Cloudflare Tunnel, ingress) saw. *)
let client_ip request =
  let from_header name =
    match header request name with
    | Some value when String.trim value <> "" ->
        Some (String.trim (List.hd (String.split_on_char ',' value)))
    | _ -> None
  in
  match List.find_map from_header [ "cf-connecting-ip"; "x-forwarded-for"; "x-real-ip" ] with
  | Some ip -> ip
  | None -> "-"

let relay_url request =
  match Sys.getenv_opt "RELAY_URL" with
  | Some url when url <> "" -> url
  | _ -> ( match header request "host" with Some host -> "wss://" ^ host | None -> "")

let contains ~needle haystack =
  let n = String.length needle and h = String.length haystack in
  let rec loop i = i + n <= h && (String.sub haystack i n = needle || loop (i + 1)) in
  n = 0 || loop 0

let websocket request =
  let messages, push_message = Lwt_stream.create () in
  let ended = ref false in
  let finish () =
    if not !ended then begin
      ended := true;
      push_message None
    end
  in
  let push_frame = ref (fun (_ : Frame.t option) -> ()) in
  (* Text messages may arrive fragmented across frames. *)
  let pending = Buffer.create 256 in
  let deliver content =
    if Buffer.length pending = 0 then push_message (Some content)
    else begin
      Buffer.add_string pending content;
      push_message (Some (Buffer.contents pending));
      Buffer.clear pending
    end
  in
  let on_frame (frame : Frame.t) =
    match frame.opcode with
    | Frame.Opcode.Text | Frame.Opcode.Binary ->
        if frame.final then deliver frame.content else Buffer.add_string pending frame.content
    | Frame.Opcode.Continuation ->
        if frame.final then deliver frame.content else Buffer.add_string pending frame.content
    | Frame.Opcode.Ping ->
        !push_frame (Some (Frame.create ~opcode:Frame.Opcode.Pong ~content:frame.content ()))
    | Frame.Opcode.Close ->
        finish ();
        !push_frame None
    | _ -> ()
  in
  Websocket_cohttp_lwt.upgrade_connection request on_frame
  >>= fun (response_action, frames_out) ->
  push_frame := frames_out;
  let ip = client_ip request in
  let conn =
    Relay.create ~ip ~relay_url:(relay_url request)
      ~push:(fun text -> frames_out (Some (Frame.create ~opcode:Frame.Opcode.Text ~content:text ())))
      ~shutdown:(fun () -> frames_out None)
  in
  Log.info "[%s] client connected" ip;
  (* The library lets read errors escape the connection handler, so wrap it to
     learn about resets and half closed sockets and end the message stream. *)
  let response_action =
    match response_action with
    | `Expert (response, handler) ->
        `Expert
          ( response,
            fun ic oc ->
              Lwt.finalize
                (fun () -> Lwt.catch (fun () -> handler ic oc) (fun _ -> Lwt.return_unit))
                (fun () ->
                  finish ();
                  Lwt.return_unit) )
    | action -> action
  in
  Lwt.async (fun () ->
      Lwt.finalize
        (fun () ->
          Lwt.catch
            (fun () ->
              Relay.auth_challenge conn >>= fun () ->
              Lwt_stream.iter_s
                (fun message ->
                  Log.info "[%s] %s" ip message;
                  Relay.handle conn message)
                messages)
            (fun exn ->
              Log.error "[%s] connection failed: %s" ip (Printexc.to_string exn);
              Lwt.return_unit))
        (fun () ->
          Relay.close conn;
          Log.info "[%s] client disconnected" ip;
          Lwt.return_unit));
  Lwt.return response_action

let nip11 () =
  Cohttp_lwt_unix.Server.respond_string ~status:`OK
    ~headers:
      (Cohttp.Header.of_list
         [
           ("content-type", "application/nostr+json"); ("access-control-allow-origin", "*");
         ])
    ~body:(Yojson.Safe.to_string (Nip11.document ()))
    ()
  >|= fun response -> `Response response

let content_type path =
  match Filename.extension path with
  | ".html" -> "text/html; charset=utf-8"
  | ".js" -> "text/javascript; charset=utf-8"
  | ".css" -> "text/css; charset=utf-8"
  | ".json" -> "application/json"
  | ".png" -> "image/png"
  | ".jpg" | ".jpeg" -> "image/jpeg"
  | ".gif" -> "image/gif"
  | ".svg" -> "image/svg+xml"
  | ".ico" -> "image/x-icon"
  | ".txt" -> "text/plain; charset=utf-8"
  | _ -> "application/octet-stream"

let not_found () =
  Cohttp_lwt_unix.Server.respond_string ~status:`Not_found ~body:"Not Found\n" ()
  >|= fun response -> `Response response

let static path =
  let path = if path = "/" then "/index.html" else path in
  if contains ~needle:".." path || not (String.length path > 0 && path.[0] = '/') then not_found ()
  else begin
    let file = Filename.concat "public" (String.sub path 1 (String.length path - 1)) in
    if Sys.file_exists file && not (Sys.is_directory file) then
      Cohttp_lwt_unix.Server.respond_file
        ~headers:(Cohttp.Header.init_with "content-type" (content_type file))
        ~fname:file ()
      >|= fun response -> `Response response
    else not_found ()
  end

let callback _conn request body =
  Cohttp_lwt.Body.drain_body body >>= fun () ->
  let path = Uri.pct_decode (Uri.path (Cohttp.Request.uri request)) in
  let upgrade = Option.value ~default:"" (header request "upgrade") in
  let accept = Option.value ~default:"" (header request "accept") in
  if String.lowercase_ascii upgrade = "websocket" then websocket request
  else if (path = "/" || path = "") && contains ~needle:"application/nostr+json" accept then nip11 ()
  else static path

let () =
  Lwt_main.run
    (Lwt.catch Store.init (fun exn ->
         Log.error "failed to initialize schema: %s" (Printexc.to_string exn);
         Lwt.return_unit));
  Log.info "listening on 0.0.0.0:%d" port;
  Lwt_main.run
    (Cohttp_lwt_unix.Server.create
       ~mode:(`TCP (`Port port))
       (Cohttp_lwt_unix.Server.make_response_action ~callback ()))
