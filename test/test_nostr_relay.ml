open Nostr_relay

let failures = ref 0

let check name condition =
  if condition then Printf.printf "ok   - %s\n" name
  else begin
    incr failures;
    Printf.printf "FAIL - %s\n" name
  end

(* Signed with nak, secret key 01. The content exercises the NIP-01 escaping
   rules: quotes, a newline, a tab and non-ASCII text. *)
let signed =
  {|{"kind":1,"id":"c6f2baaadb014dde41079876e31a9a8e44b4d1ddc05beabee722b75f3e1f73d6","pubkey":"79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798","created_at":1700000000,"tags":[["t","ocaml"]],"content":"hello \"ocaml\"\nこんにちは\ttab","sig":"1c483bc1052eb13eb55356c11eb50011250b48fbbe3c278ab9abe657502451c71fc04fd4d849cdc4fcbf0fb4c6a29dee6cf6fca9df38028ef0fc4a203d3b109a"}|}

let event = Event.of_json (Yojson.Safe.from_string signed)

let filter_of s = Filter.of_json (Yojson.Safe.from_string s)

let () =
  check "event id is derived from the canonical serialization"
    (Util.to_hex (Event.digest event) = event.id);
  check "valid signature is accepted" (Event.verify event);
  check "tampered content is rejected"
    (not (Event.verify { event with content = event.content ^ "!" }));
  check "tampered signature is rejected"
    (not
       (Event.verify
          {
            event with
            signature = String.map (function '1' -> '2' | c -> c) event.signature;
          }));
  check "id must be lower case hex" (not (Event.verify { event with id = String.uppercase_ascii event.id }));

  check "kind filter matches" (Filter.matches event (filter_of {|{"kinds":[1]}|}));
  check "kind filter rejects" (not (Filter.matches event (filter_of {|{"kinds":[0,7]}|})));
  check "author prefix matches"
    (Filter.matches event (filter_of {|{"authors":["79be667e"]}|}));
  check "author prefix rejects"
    (not (Filter.matches event (filter_of {|{"authors":["deadbeef"]}|})));
  check "tag filter matches" (Filter.matches event (filter_of {|{"#t":["ocaml"]}|}));
  check "tag filter rejects" (not (Filter.matches event (filter_of {|{"#t":["nim"]}|})));
  check "tag filter is name sensitive"
    (not (Filter.matches event (filter_of {|{"#p":["ocaml"]}|})));
  check "since filter rejects older events"
    (not (Filter.matches event (filter_of {|{"since":1800000000}|})));
  check "until filter matches" (Filter.matches event (filter_of {|{"until":1800000000}|}));

  (* NIP-26 conditions *)
  check "delegation without a kind condition applies to every kind"
    (Nip26.conditions_allow event "created_at>1600000000");
  check "delegation enumerates permitted kinds"
    (Nip26.conditions_allow event "kind=0&kind=1");
  check "delegation rejects other kinds" (not (Nip26.conditions_allow event "kind=0"));
  check "delegation rejects events after created_at<"
    (not (Nip26.conditions_allow event "kind=1&created_at<1600000000"));
  check "delegation rejects events before created_at>"
    (not (Nip26.conditions_allow event "kind=1&created_at>1800000000"));
  check "event without a delegation tag is valid" (Nip26.validate event);
  check "event with a bogus delegation tag is rejected"
    (not
       (Nip26.validate
          { event with tags = [ [ "delegation"; "deadbeef"; "kind=1"; "00" ] ] }));

  check "protected events are detected"
    (Event.is_protected { event with tags = [ [ "-" ] ] });
  check "expiration in the past is detected"
    (Event.is_expired { event with tags = [ [ "expiration"; "1700000000" ] ] });
  check "expiration in the future is not expired"
    (not (Event.is_expired { event with tags = [ [ "expiration"; "4000000000" ] ] }));
  check "malformed messages are rejected"
    (match Event.of_json (Yojson.Safe.from_string {|{"id":1}|}) with
    | _ -> false
    | exception Event.Invalid _ -> true);

  if !failures > 0 then begin
    Printf.printf "%d failure(s)\n" !failures;
    exit 1
  end
