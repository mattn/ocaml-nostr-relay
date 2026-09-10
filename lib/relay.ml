open Lwt.Infix

type t = {
  id : int;
  ip : string;
  push : string -> unit;  (** queues a text frame for the client *)
  shutdown : unit -> unit;  (** closes the connection *)
  challenge : string;
  relay_url : string;
  mutable authed : string list;  (** NIP-42 authenticated pubkeys *)
  subs : (string, Filter.t list) Hashtbl.t;
}

let connections : (int, t) Hashtbl.t = Hashtbl.create 16
let last_id = ref 0

let create ~ip ~relay_url ~push ~shutdown =
  incr last_id;
  let conn =
    {
      id = !last_id;
      ip;
      push;
      shutdown;
      challenge = Util.random_hex 32;
      relay_url;
      authed = [];
      subs = Hashtbl.create 8;
    }
  in
  Hashtbl.replace connections conn.id conn;
  conn

let close conn = Hashtbl.remove connections conn.id

let send conn json =
  (try conn.push (Yojson.Safe.to_string json) with _ -> ());
  Lwt.return_unit

let ok conn id accepted message =
  send conn (`List [ `String "OK"; `String id; `Bool accepted; `String message ])

let notice conn message = send conn (`List [ `String "NOTICE"; `String message ])
let eose conn sub = send conn (`List [ `String "EOSE"; `String sub ])

let closed conn sub message =
  send conn (`List [ `String "CLOSED"; `String sub; `String message ])

let send_event conn sub (ev : Event.t) =
  send conn (`List [ `String "EVENT"; `String sub; Event.to_json ev ])

let auth_challenge conn = send conn (`List [ `String "AUTH"; `String conn.challenge ])

(* NIP-59: gift wraps only reach the authenticated recipient. *)
let visible_to conn (ev : Event.t) =
  ev.kind <> 1059 || List.exists (fun p -> List.mem p conn.authed) (Event.tag_values ev "p")

let broadcast (ev : Event.t) =
  Hashtbl.fold (fun _ conn acc -> conn :: acc) connections []
  |> Lwt_list.iter_p (fun conn ->
         if not (visible_to conn ev) then Lwt.return_unit
         else
           Hashtbl.fold (fun sub filters acc -> (sub, filters) :: acc) conn.subs []
           |> Lwt_list.iter_s (fun (sub, filters) ->
                  if List.exists (Filter.matches ev) filters then send_event conn sub ev
                  else Lwt.return_unit))

(* NIP-09 *)
let delete_targets (ev : Event.t) =
  Event.tag_values ev "e"
  |> Lwt_list.iter_s (fun id ->
         Store.get_by_id id >>= function
         | None -> Lwt.return_unit
         | Some target when target.kind = 1059 ->
             Store.delete_wrap_by_id_and_recipient ~id ~pubkey:ev.pubkey
         | Some _ -> Store.delete_by_id_and_author ~id ~pubkey:ev.pubkey)

let store (ev : Event.t) =
  if ev.kind = 5 then delete_targets ev
  else if Event.is_ephemeral ev then Lwt.return_unit
  else
    (if Event.is_replaceable ev then
       Store.delete_replaceable ~kind:ev.kind ~pubkey:ev.pubkey ~created_at:ev.created_at
     else if Event.is_addressable ev then
       match Event.first_tag_value ev "d" with
       | Some dtag when dtag <> "" ->
           Store.delete_addressable ~kind:ev.kind ~pubkey:ev.pubkey ~dtag
             ~created_at:ev.created_at
       | _ -> Lwt.return_unit
     else Lwt.return_unit)
    >>= fun () -> Store.save ev

let do_event conn (ev : Event.t) =
  if not (Event.verify ev) then ok conn ev.id false "invalid: signature verification failed"
  else if not (Nip26.validate ev) then
    ok conn ev.id false "invalid: delegation verification failed"
  else if Event.is_protected ev && not (List.mem ev.pubkey conn.authed) then
    ok conn ev.id false "auth-required: this event may only be published by its author"
  else
    Lwt.catch
      (fun () -> store ev >>= fun () -> ok conn ev.id true "" >>= fun () -> broadcast ev)
      (fun exn ->
        Log.error "failed to store event: %s" (Printexc.to_string exn);
        ok conn ev.id false "error: failed to store event")

let do_req conn sub filters =
  Hashtbl.replace conn.subs sub filters;
  Lwt.catch
    (fun () ->
      filters
      |> Lwt_list.iter_s (fun filter ->
             Store.query filter ~authed:conn.authed
             >>= Lwt_list.iter_s (fun ev -> send_event conn sub ev))
      >>= fun () -> eose conn sub)
    (fun exn ->
      Log.error "failed to query events: %s" (Printexc.to_string exn);
      Hashtbl.remove conn.subs sub;
      closed conn sub "error: could not query events")

(* NIP-45 *)
let do_count conn sub filters =
  Lwt.catch
    (fun () ->
      Lwt_list.fold_left_s
        (fun total filter -> Store.count filter ~authed:conn.authed >|= ( + ) total)
        0 filters
      >>= fun total ->
      send conn (`List [ `String "COUNT"; `String sub; `Assoc [ ("count", `Int total) ] ]))
    (fun exn ->
      Log.error "failed to count events: %s" (Printexc.to_string exn);
      closed conn sub "error: could not count events")

let normalize_relay_url url =
  let url = String.lowercase_ascii url in
  let rec trim url =
    if String.length url > 0 && url.[String.length url - 1] = '/' then
      trim (String.sub url 0 (String.length url - 1))
    else url
  in
  trim url

(* NIP-42 *)
let do_auth conn (ev : Event.t) =
  let reject reason = ok conn ev.id false ("invalid: " ^ reason) in
  let tag_matches name value =
    List.exists (fun v -> v = value) (Event.tag_values ev name)
  in
  if ev.kind <> 22242 then reject "authentication event must be kind 22242"
  else if abs (Util.now () - ev.created_at) > 600 then
    reject "authentication event timestamp is out of range"
  else if not (tag_matches "challenge" conn.challenge) then
    reject "authentication challenge does not match"
  else if
    not
      (List.exists
         (fun url -> normalize_relay_url url = normalize_relay_url conn.relay_url)
         (Event.tag_values ev "relay"))
  then reject "authentication relay does not match"
  else if not (Event.verify ev) then reject "authentication signature verification failed"
  else begin
    if not (List.mem ev.pubkey conn.authed) then conn.authed <- ev.pubkey :: conn.authed;
    ok conn ev.id true ""
  end

let dispatch conn message =
  match Yojson.Safe.from_string message with
  | `List (`String kind :: rest) -> (
      match (kind, rest) with
      | "EVENT", [ json ] -> do_event conn (Event.of_json json)
      | "AUTH", [ json ] -> do_auth conn (Event.of_json json)
      | "REQ", `String sub :: filters -> do_req conn sub (List.map Filter.of_json filters)
      | "COUNT", `String sub :: filters -> do_count conn sub (List.map Filter.of_json filters)
      | "CLOSE", [ `String sub ] ->
          Hashtbl.remove conn.subs sub;
          Lwt.return_unit
      | _ -> notice conn ("invalid: malformed " ^ kind ^ " message"))
  | _ -> notice conn "invalid: message must be a JSON array"

let handle conn message =
  Lwt.catch
    (fun () -> dispatch conn message)
    (function
      | Event.Invalid reason -> notice conn ("invalid: " ^ reason)
      | Yojson.Json_error reason -> notice conn ("invalid: " ^ reason)
      | exn -> notice conn ("error: " ^ Printexc.to_string exn))
