let hex_digits = "0123456789abcdef"

let to_hex (s : string) : string =
  let buf = Bytes.create (String.length s * 2) in
  String.iteri
    (fun i c ->
      let n = Char.code c in
      Bytes.set buf (i * 2) hex_digits.[n lsr 4];
      Bytes.set buf ((i * 2) + 1) hex_digits.[n land 0x0f])
    s;
  Bytes.unsafe_to_string buf

let hex_value = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | 'a' .. 'f' as c -> Char.code c - Char.code 'a' + 10
  | 'A' .. 'F' as c -> Char.code c - Char.code 'A' + 10
  | _ -> -1

let of_hex (s : string) : string option =
  let len = String.length s in
  if len mod 2 <> 0 then None
  else begin
    let buf = Bytes.create (len / 2) in
    let ok = ref true in
    for i = 0 to (len / 2) - 1 do
      let hi = hex_value s.[i * 2] and lo = hex_value s.[(i * 2) + 1] in
      if hi < 0 || lo < 0 then ok := false
      else Bytes.set buf i (Char.chr ((hi lsl 4) lor lo))
    done;
    if !ok then Some (Bytes.unsafe_to_string buf) else None
  end

let is_hex ~len s =
  String.length s = len && String.for_all (fun c -> hex_value c >= 0) s

let now () = int_of_float (Unix.time ())

let random_hex n =
  let ic = open_in_bin "/dev/urandom" in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () -> to_hex (really_input_string ic n))

let getenv ?(default = "") name =
  match Sys.getenv_opt name with Some v when v <> "" -> v | _ -> default

let has_prefix ~prefix s = String.starts_with ~prefix s

(* NIP-50: split a search string into whitespace separated words. An event
   matches when every word occurs in its content, which is what both the SQL
   query and Filter.matches check, so stored and live results agree. *)
let search_words (search : string) : string list =
  let words = ref [] and buf = Buffer.create 16 in
  let flush () =
    if Buffer.length buf > 0 then begin
      words := Buffer.contents buf :: !words;
      Buffer.clear buf
    end
  in
  String.iter
    (fun c -> match c with ' ' | '\t' | '\n' | '\r' -> flush () | _ -> Buffer.add_char buf c)
    search;
  flush ();
  List.rev !words

(* Escape the LIKE wildcards so a search for "%" cannot match everything. *)
let escape_like (word : string) : string =
  let buf = Buffer.create (String.length word) in
  String.iter
    (fun c ->
      (match c with '\\' | '%' | '_' -> Buffer.add_char buf '\\' | _ -> ());
      Buffer.add_char buf c)
    word;
  Buffer.contents buf

(* ASCII lowercase; the non-ASCII bytes that most searches are made of are
   left alone. *)
let lowercase_ascii = String.lowercase_ascii

let contains_ignore_ascii_case ~(needle : string) (haystack : string) : bool =
  let needle = lowercase_ascii needle and haystack = lowercase_ascii haystack in
  let n = String.length needle and h = String.length haystack in
  if n = 0 then true
  else if n > h then false
  else begin
    let found = ref false and i = ref 0 in
    while (not !found) && !i <= h - n do
      if String.sub haystack !i n = needle then found := true;
      incr i
    done;
    !found
  end
