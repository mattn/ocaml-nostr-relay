type t = {
  id : string;
  pubkey : string;
  created_at : int;
  kind : int;
  tags : string list list;
  content : string;
  signature : string;
}

exception Invalid of string

let invalid fmt = Printf.ksprintf (fun msg -> raise (Invalid msg)) fmt

let member name (json : Yojson.Safe.t) =
  match json with
  | `Assoc fields -> ( match List.assoc_opt name fields with Some v -> v | None -> `Null)
  | _ -> `Null

let as_string name (json : Yojson.Safe.t) =
  match json with `String s -> s | _ -> invalid "%s must be a string" name

let as_int name (json : Yojson.Safe.t) =
  match json with
  | `Int i -> i
  | `Intlit s -> ( match int_of_string_opt s with Some i -> i | None -> invalid "%s must be a number" name)
  | `Float f -> int_of_float f
  | _ -> invalid "%s must be a number" name

let tags_of_json (json : Yojson.Safe.t) =
  match json with
  | `Null -> []
  | `List tags ->
      List.map
        (function
          | `List values -> List.map (as_string "tag value") values
          | _ -> invalid "tags must be arrays of strings")
        tags
  | _ -> invalid "tags must be an array"

let of_json (json : Yojson.Safe.t) : t =
  {
    id = as_string "id" (member "id" json);
    pubkey = as_string "pubkey" (member "pubkey" json);
    created_at = as_int "created_at" (member "created_at" json);
    kind = as_int "kind" (member "kind" json);
    tags = tags_of_json (member "tags" json);
    content = as_string "content" (member "content" json);
    signature = as_string "sig" (member "sig" json);
  }

let json_of_tags tags : Yojson.Safe.t =
  `List (List.map (fun tag -> `List (List.map (fun v -> `String v) tag)) tags)

let to_json (ev : t) : Yojson.Safe.t =
  `Assoc
    [
      ("id", `String ev.id);
      ("pubkey", `String ev.pubkey);
      ("created_at", `Int ev.created_at);
      ("kind", `Int ev.kind);
      ("tags", json_of_tags ev.tags);
      ("content", `String ev.content);
      ("sig", `String ev.signature);
    ]

(* NIP-01 canonical string escaping. Yojson escapes more than the spec allows,
   so the serialization used for the event id is built by hand. *)
let add_escaped buf s =
  Buffer.add_char buf '"';
  String.iter
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\t' -> Buffer.add_string buf "\\t"
      | '\b' -> Buffer.add_string buf "\\b"
      | '\012' -> Buffer.add_string buf "\\f"
      | c when Char.code c < 0x20 -> Buffer.add_string buf (Printf.sprintf "\\u%04x" (Char.code c))
      | c -> Buffer.add_char buf c)
    s;
  Buffer.add_char buf '"'

let serialize (ev : t) =
  let buf = Buffer.create 256 in
  Buffer.add_string buf "[0,";
  add_escaped buf ev.pubkey;
  Buffer.add_char buf ',';
  Buffer.add_string buf (string_of_int ev.created_at);
  Buffer.add_char buf ',';
  Buffer.add_string buf (string_of_int ev.kind);
  Buffer.add_string buf ",[";
  List.iteri
    (fun i tag ->
      if i > 0 then Buffer.add_char buf ',';
      Buffer.add_char buf '[';
      List.iteri
        (fun j value ->
          if j > 0 then Buffer.add_char buf ',';
          add_escaped buf value)
        tag;
      Buffer.add_char buf ']')
    ev.tags;
  Buffer.add_string buf "],";
  add_escaped buf ev.content;
  Buffer.add_char buf ']';
  Buffer.contents buf

let digest (ev : t) = Digestif.SHA256.(to_raw_string (digest_string (serialize ev)))

let verify (ev : t) =
  Util.is_hex ~len:64 ev.id
  && Util.is_hex ~len:64 ev.pubkey
  && Util.is_hex ~len:128 ev.signature
  &&
  let digest = digest ev in
  Util.to_hex digest = ev.id
  && Schnorr.verify ~sig_hex:ev.signature ~pubkey_hex:ev.pubkey digest

let tag_values (ev : t) name =
  List.filter_map (function n :: v :: _ when n = name -> Some v | _ -> None) ev.tags

let first_tag_value (ev : t) name =
  match tag_values ev name with value :: _ -> Some value | [] -> None

(* NIP-26: an event signed by a delegatee counts as authored by the delegator. *)
let delegated_by (ev : t) prefix =
  List.exists
    (function
      | "delegation" :: delegator :: _ :: _ :: _ -> Util.has_prefix ~prefix delegator
      | _ -> false)
    ev.tags

let delegation_tag (ev : t) =
  List.find_map
    (function
      | "delegation" :: delegator :: conditions :: signature :: _ ->
          Some (delegator, conditions, signature)
      | _ -> None)
    ev.tags

(* NIP-70 *)
let is_protected (ev : t) = List.exists (function "-" :: _ -> true | _ -> false) ev.tags

(* NIP-40 *)
let is_expired (ev : t) =
  match first_tag_value ev "expiration" with
  | Some value -> (
      match int_of_string_opt value with Some at -> at <= Util.now () | None -> false)
  | None -> false

let is_ephemeral (ev : t) = ev.kind >= 20000 && ev.kind < 30000
let is_replaceable (ev : t) = ev.kind = 0 || ev.kind = 3 || (ev.kind >= 10000 && ev.kind < 20000)
let is_addressable (ev : t) = ev.kind >= 30000 && ev.kind < 40000
