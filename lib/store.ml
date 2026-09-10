open Lwt.Infix

(* A single libpq connection, guarded by a mutex and driven from a worker
   thread so that blocking queries never stall the Lwt event loop. *)

let mutex = Lwt_mutex.create ()
let conn : Postgresql.connection option ref = ref None

let conninfo () =
  match Sys.getenv_opt "DATABASE_URL" with
  | Some url when url <> "" -> url
  | _ ->
      Printf.sprintf "host=%s user=%s password=%s dbname=%s"
        (Util.getenv ~default:"localhost" "DB_HOST")
        (Util.getenv ~default:"postgres" "DB_USER")
        (Util.getenv "DB_PASS")
        (Util.getenv ~default:"nostr" "DB_NAME")

let disconnect () =
  (match !conn with Some c -> ( try c#finish with _ -> ()) | None -> ());
  conn := None

let connect () =
  disconnect ();
  let c = new Postgresql.connection ~conninfo:(conninfo ()) () in
  conn := Some c;
  c

let current () =
  match !conn with Some c when c#status = Postgresql.Ok -> c | _ -> connect ()

let run (c : Postgresql.connection) query params =
  (c#exec ~expect:[ Postgresql.Command_ok; Postgresql.Tuples_ok ] ~params query)#get_all

let blocking_exec query params =
  try run (current ()) query params
  with Postgresql.Error _ -> run (connect ()) query params

let exec ?(params = [||]) query =
  Lwt_mutex.with_lock mutex (fun () ->
      Lwt_preemptive.detach (fun () -> blocking_exec query params) ())

let schema =
  [
    {|CREATE OR REPLACE FUNCTION tags_to_tagvalues(jsonb) RETURNS text[]
    AS 'SELECT array_agg(t->>1) FROM (SELECT jsonb_array_elements($1) AS t)s WHERE length(t->>0) = 1;'
    LANGUAGE SQL
    IMMUTABLE
    RETURNS NULL ON NULL INPUT|};
    {|CREATE TABLE IF NOT EXISTS event (
  id text NOT NULL,
  pubkey text NOT NULL,
  created_at integer NOT NULL,
  kind integer NOT NULL,
  tags jsonb NOT NULL,
  content text NOT NULL,
  sig text NOT NULL,

  tagvalues text[] GENERATED ALWAYS AS (tags_to_tagvalues(tags)) STORED
)|};
    "CREATE UNIQUE INDEX IF NOT EXISTS ididx ON event USING btree (id text_pattern_ops)";
    "CREATE INDEX IF NOT EXISTS pubkeyprefix ON event USING btree (pubkey text_pattern_ops)";
    "CREATE INDEX IF NOT EXISTS timeidx ON event (created_at DESC)";
    "CREATE INDEX IF NOT EXISTS kindidx ON event (kind)";
    "CREATE INDEX IF NOT EXISTS kindtimeidx ON event(kind,created_at DESC)";
    "CREATE INDEX IF NOT EXISTS arbitrarytagvalues ON event USING gin (tagvalues)";
    (* NIP-50: search is a substring match, so a trigram index keeps the leading
       wildcard off a sequential scan. Terms shorter than 3 characters produce no
       trigrams and still fall back to a scan. *)
    "CREATE EXTENSION IF NOT EXISTS pg_trgm";
    "CREATE INDEX IF NOT EXISTS contenttrgmidx ON event USING gin (content gin_trgm_ops)";
  ]

let init () = Lwt_list.iter_s (fun sql -> exec sql >|= ignore) schema

let event_of_row (row : string array) : Event.t =
  {
    Event.id = row.(0);
    pubkey = row.(1);
    created_at = int_of_string row.(2);
    kind = int_of_string row.(3);
    tags = Event.tags_of_json (Yojson.Safe.from_string row.(4));
    content = row.(5);
    signature = row.(6);
  }

let save (ev : Event.t) =
  exec
    ~params:
      [|
        ev.id;
        ev.pubkey;
        string_of_int ev.created_at;
        string_of_int ev.kind;
        Yojson.Safe.to_string (Event.json_of_tags ev.tags);
        ev.content;
        ev.signature;
      |]
    {|INSERT INTO event (id, pubkey, created_at, kind, tags, content, sig)
      VALUES ($1, $2, $3, $4, $5::jsonb, $6, $7)
      ON CONFLICT (id) DO NOTHING|}
  >|= ignore

let get_by_id id =
  exec ~params:[| id |]
    "SELECT id, pubkey, created_at, kind, tags, content, sig FROM event WHERE id = $1"
  >|= fun rows -> if Array.length rows = 0 then None else Some (event_of_row rows.(0))

(* NIP-09: an author may delete their own events, including ones they signed
   through a NIP-26 delegation. *)
let delete_by_id_and_author ~id ~pubkey =
  exec ~params:[| id; pubkey; pubkey |]
    {|DELETE FROM event
      WHERE id = $1 AND (
        pubkey = $2 OR EXISTS (
          SELECT 1 FROM jsonb_array_elements(tags) tag
          WHERE tag->>0 = 'delegation' AND tag->>1 = $3
        )
      )|}
  >|= ignore

(* NIP-59: a gift wrap can only be deleted by its recipient. *)
let delete_wrap_by_id_and_recipient ~id ~pubkey =
  exec ~params:[| id; pubkey |]
    {|DELETE FROM event
      WHERE id = $1 AND kind = 1059
        AND EXISTS (
          SELECT 1 FROM jsonb_array_elements(tags) tag
          WHERE tag->>0 = 'p' AND tag->>1 = $2
        )|}
  >|= ignore

let delete_replaceable ~kind ~pubkey ~created_at =
  exec
    ~params:[| string_of_int kind; pubkey; string_of_int created_at |]
    "DELETE FROM event WHERE kind = $1::int AND pubkey = $2 AND created_at <= $3::int"
  >|= ignore

let delete_addressable ~kind ~pubkey ~dtag ~created_at =
  exec
    ~params:
      [|
        string_of_int kind;
        pubkey;
        Yojson.Safe.to_string (`List [ `String "d"; `String dtag ]);
        string_of_int created_at;
      |]
    {|DELETE FROM event
      WHERE kind = $1::int AND pubkey = $2 AND tags @> $3::jsonb AND created_at <= $4::int|}
  >|= ignore

(* Builds the WHERE clause of a query, together with its bind parameters. *)
let build_where (filter : Filter.t) ~authed =
  let params = ref [] and count = ref 0 in
  let param value =
    incr count;
    params := value :: !params;
    "$" ^ string_of_int !count
  in
  let clauses = ref [] in
  let add clause = clauses := clause :: !clauses in
  let any_of values render =
    if values = [] then add "false"
    else add ("(" ^ String.concat " OR " (List.map render values) ^ ")")
  in
  (match filter.ids with
  | None -> ()
  | Some ids -> any_of ids (fun id -> "id LIKE " ^ param id ^ " || '%'"));
  (match filter.authors with
  | None -> ()
  | Some authors ->
      any_of authors (fun author ->
          let a = param author and b = param author in
          "(pubkey LIKE " ^ a
          ^ " || '%' OR EXISTS (SELECT 1 FROM jsonb_array_elements(tags) tag WHERE tag->>0 \
             = 'delegation' AND tag->>1 LIKE " ^ b ^ " || '%'))"));
  (match filter.kinds with
  | None -> ()
  | Some [] -> add "false"
  | Some kinds -> add ("kind IN (" ^ String.concat "," (List.map string_of_int kinds) ^ ")"));
  (match filter.since with
  | None -> ()
  | Some since -> add ("created_at >= " ^ string_of_int since));
  (match filter.until with
  | None -> ()
  | Some until -> add ("created_at <= " ^ string_of_int until));
  (* NIP-50: every word of the search string must occur in the content. *)
  (match filter.search with
  | None -> ()
  | Some search ->
      List.iter
        (fun word ->
          add ("content ILIKE " ^ param ("%" ^ Util.escape_like word ^ "%") ^ " ESCAPE '\\'"))
        (Util.search_words search));
  List.iter
    (fun (name, values) ->
      if values = [] then add "false"
      else begin
        let array = "ARRAY[" ^ String.concat "," (List.map param values) ^ "]::text[]" in
        let exact =
          "EXISTS (SELECT 1 FROM jsonb_array_elements(tags) tag WHERE tag->>0 = " ^ param name
          ^ " AND tag->>1 = ANY(" ^ array ^ "))"
        in
        (* tagvalues only holds the values of single letter tags, so the GIN
           index can narrow those before the tag name is checked exactly. *)
        if String.length name = 1 then add ("(tagvalues && " ^ array ^ " AND " ^ exact ^ ")")
        else add exact
      end)
    filter.tags;
  (* NIP-40: expired events are never served, and must be excluded before
     LIMIT is applied. *)
  add
    (Printf.sprintf
       "NOT EXISTS (SELECT 1 FROM jsonb_array_elements(tags) tag WHERE tag->>0 = 'expiration' \
        AND (CASE WHEN tag->>1 ~ '^[0-9]{1,18}$' THEN (tag->>1)::bigint END) <= %d)"
       (Util.now ()));
  (* NIP-59: gift wraps are only visible to authenticated recipients. *)
  (match authed with
  | [] -> add "kind <> 1059"
  | pubkeys ->
      add
        ("(kind <> 1059 OR EXISTS (SELECT 1 FROM jsonb_array_elements(tags) tag WHERE \
          tag->>0 = 'p' AND tag->>1 IN ("
        ^ String.concat "," (List.map param pubkeys)
        ^ ")))"));
  let where =
    match List.rev !clauses with [] -> "" | clauses -> " WHERE " ^ String.concat " AND " clauses
  in
  (where, Array.of_list (List.rev !params))

let query (filter : Filter.t) ~authed =
  let where, params = build_where filter ~authed in
  let limit = match filter.limit with Some l when l >= 0 && l <= 1000 -> l | _ -> 500 in
  exec ~params
    ("SELECT id, pubkey, created_at, kind, tags, content, sig FROM event" ^ where
   ^ " ORDER BY created_at DESC LIMIT " ^ string_of_int limit)
  >|= fun rows ->
  Array.to_list rows |> List.map event_of_row |> List.filter (fun ev -> not (Event.is_expired ev))

(* NIP-45 *)
let count (filter : Filter.t) ~authed =
  let where, params = build_where filter ~authed in
  exec ~params ("SELECT id, tags FROM event" ^ where) >|= fun rows ->
  Array.fold_left
    (fun ids row ->
      let expired =
        List.exists
          (function
            | "expiration" :: value :: _ -> (
                match int_of_string_opt value with Some at -> at <= Util.now () | None -> false)
            | _ -> false)
          (Event.tags_of_json (Yojson.Safe.from_string row.(1)))
      in
      if expired then ids else row.(0) :: ids)
    [] rows
  |> List.sort_uniq compare |> List.length
