(* NIP-26: Delegated Event Signing *)

let find_operator condition =
  let len = String.length condition in
  let rec loop i =
    if i >= len then None
    else match condition.[i] with '=' | '<' | '>' -> Some i | _ -> loop (i + 1)
  in
  loop 0

(* Conditions are AND-combined, except that repeated conditions on the same
   field enumerate permitted values (e.g. "kind=0&kind=1" permits both). A
   delegation without any kind condition applies to every kind. *)
let conditions_allow (ev : Event.t) conditions =
  let has_kind = ref false and kind_matched = ref false and ok = ref true in
  let apply condition =
    match find_operator condition with
    | None -> ()
    | Some pos -> (
        let key = String.sub condition 0 pos in
        let op = condition.[pos] in
        let value = String.sub condition (pos + 1) (String.length condition - pos - 1) in
        match (key, op) with
        | "kind", '=' ->
            has_kind := true;
            if int_of_string_opt value = Some ev.kind then kind_matched := true
        | "created_at", '<' -> (
            match int_of_string_opt value with
            | Some until -> if ev.created_at >= until then ok := false
            | None -> ok := false)
        | "created_at", '>' -> (
            match int_of_string_opt value with
            | Some since -> if ev.created_at <= since then ok := false
            | None -> ok := false)
        | _ -> ())
  in
  List.iter apply (String.split_on_char '&' conditions);
  !ok && ((not !has_kind) || !kind_matched)

let token_digest ~delegatee ~conditions =
  let token = "nostr:delegation:" ^ delegatee ^ ":" ^ conditions in
  Digestif.SHA256.(to_raw_string (digest_string token))

(* Returns true when the event carries no delegation tag, or a valid one. *)
let validate (ev : Event.t) =
  match Event.delegation_tag ev with
  | None -> true
  | Some (delegator, conditions, signature) ->
      Util.is_hex ~len:64 delegator
      && Util.is_hex ~len:128 signature
      && conditions <> ""
      && conditions_allow ev conditions
      && Schnorr.verify ~sig_hex:signature ~pubkey_hex:delegator
           (token_digest ~delegatee:ev.pubkey ~conditions)
