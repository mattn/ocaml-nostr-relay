type t = {
  ids : string list option;
  authors : string list option;
  kinds : int list option;
  tags : (string * string list) list;  (** tag name without the leading '#' *)
  since : int option;
  until : int option;
  search : string option;  (** NIP-50 *)
  limit : int option;
}

let empty =
  { ids = None; authors = None; kinds = None; tags = []; since = None; until = None;
    search = None; limit = None }

let string_list name (json : Yojson.Safe.t) =
  match json with
  | `List values -> List.map (Event.as_string name) values
  | _ -> Event.invalid "%s must be an array" name

let int_list name (json : Yojson.Safe.t) =
  match json with
  | `List values -> List.map (Event.as_int name) values
  | _ -> Event.invalid "%s must be an array" name

let of_json (json : Yojson.Safe.t) : t =
  match json with
  | `Assoc fields ->
      List.fold_left
        (fun filter (name, value) ->
          match name with
          | "ids" -> { filter with ids = Some (string_list name value) }
          | "authors" -> { filter with authors = Some (string_list name value) }
          | "kinds" -> { filter with kinds = Some (int_list name value) }
          | "since" -> { filter with since = Some (Event.as_int name value) }
          | "until" -> { filter with until = Some (Event.as_int name value) }
          | "search" -> { filter with search = Some (Event.as_string name value) }
          | "limit" -> { filter with limit = Some (Event.as_int name value) }
          | _ when String.length name >= 2 && name.[0] = '#' ->
              let key = String.sub name 1 (String.length name - 1) in
              { filter with tags = (key, string_list name value) :: filter.tags }
          | _ -> filter)
        empty fields
  | _ -> Event.invalid "filter must be an object"

let matches (ev : Event.t) (filter : t) =
  let any_prefix_of value prefixes =
    List.exists (fun prefix -> Util.has_prefix ~prefix value) prefixes
  in
  (match filter.ids with None -> true | Some ids -> any_prefix_of ev.id ids)
  && (match filter.authors with
     | None -> true
     | Some authors ->
         List.exists
           (fun prefix -> Util.has_prefix ~prefix ev.pubkey || Event.delegated_by ev prefix)
           authors)
  && (match filter.kinds with None -> true | Some kinds -> List.mem ev.kind kinds)
  && (match filter.since with None -> true | Some since -> ev.created_at >= since)
  && (match filter.until with None -> true | Some until -> ev.created_at <= until)
  && (match filter.search with
     | None -> true
     | Some search ->
         List.for_all
           (fun word -> Util.contains_ignore_ascii_case ~needle:word ev.content)
           (Util.search_words search))
  && List.for_all
       (fun (name, values) ->
         List.exists (fun value -> List.mem value values) (Event.tag_values ev name))
       filter.tags
