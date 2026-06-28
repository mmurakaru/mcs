(* Parse and emit the YAML frontmatter block at the top of a note.

   Hard guarantee: the prose body is never touched. [parse] returns the body as
   a byte-exact substring; [render] concatenates a (possibly rewritten) frontmatter
   block with that untouched body. mcs only ever rewrites frontmatter, additively. *)

type value =
  | Scalar of string
  | Items of string list

(* Ordered key/value pairs - order is preserved on read so emit stays stable. *)
type t = (string * value) list

(* --- helpers --- *)

let starts_with ~prefix text =
  let prefix_length = String.length prefix in
  String.length text >= prefix_length && String.sub text 0 prefix_length = prefix

let strip = String.trim

let strip_quotes text =
  let length = String.length text in
  if length >= 2 && text.[0] = '"' && text.[length - 1] = '"' then (
    let inner = String.sub text 1 (length - 2) in
    let buffer = Buffer.create (String.length inner) in
    let index = ref 0 in
    let inner_length = String.length inner in
    while !index < inner_length do
      if inner.[!index] = '\\' && !index + 1 < inner_length then (
        (match inner.[!index + 1] with
        | '"' -> Buffer.add_char buffer '"'
        | '\\' -> Buffer.add_char buffer '\\'
        | 'n' -> Buffer.add_char buffer '\n'
        | 't' -> Buffer.add_char buffer '\t'
        | other -> Buffer.add_char buffer other);
        index := !index + 2)
      else (
        Buffer.add_char buffer inner.[!index];
        incr index)
    done;
    Buffer.contents buffer)
  else if length >= 2 && text.[0] = '\'' && text.[length - 1] = '\'' then
    String.sub text 1 (length - 2)
  else text

(* split an inline flow sequence "[a, b, c]" into its items *)
let parse_inline_list text =
  let inner = String.sub text 1 (String.length text - 2) in
  if strip inner = "" then []
  else String.split_on_char ',' inner |> List.map (fun item -> strip_quotes (strip item))

(* --- parse --- *)

let parse_fields (frontmatter_text : string) : t =
  let raw_lines = String.split_on_char '\n' frontmatter_text in
  (* drop a single trailing empty fragment produced by a final newline *)
  let raw_lines =
    match List.rev raw_lines with "" :: rest -> List.rev rest | _ -> raw_lines
  in
  let lines = Array.of_list raw_lines in
  let line_count = Array.length lines in
  let fields = ref [] in
  let index = ref 0 in
  while !index < line_count do
    let line = lines.(!index) in
    if strip line = "" then incr index
    else
      match String.index_opt line ':' with
      | None -> incr index (* not a key line; ignore *)
      | Some colon_index ->
        let key = strip (String.sub line 0 colon_index) in
        let remainder =
          strip (String.sub line (colon_index + 1) (String.length line - colon_index - 1))
        in
        if remainder = "" then (
          (* a block list may follow: subsequent lines starting with "- " *)
          let items = ref [] in
          let scan_index = ref (!index + 1) in
          let scanning = ref true in
          while !scanning && !scan_index < line_count do
            let stripped = strip lines.(!scan_index) in
            if starts_with ~prefix:"- " stripped || stripped = "-" then (
              let item = strip (String.sub stripped 1 (String.length stripped - 1)) in
              items := strip_quotes item :: !items;
              incr scan_index)
            else scanning := false
          done;
          if !items = [] then (
            fields := (key, Scalar "") :: !fields;
            incr index)
          else (
            fields := (key, Items (List.rev !items)) :: !fields;
            index := !scan_index))
        else if
          starts_with ~prefix:"[" remainder
          && String.length remainder >= 2
          && remainder.[String.length remainder - 1] = ']'
        then (
          fields := (key, Items (parse_inline_list remainder)) :: !fields;
          incr index)
        else (
          fields := (key, Scalar (strip_quotes remainder)) :: !fields;
          incr index)
  done;
  List.rev !fields

(* Split file content into (frontmatter, body). Body is a byte-exact substring.
   A file that does not open with a "---" line, or whose block is unterminated,
   is treated as having no frontmatter - "not yet reconciled", not an error. *)
let parse (content : string) : t option * string =
  let length = String.length content in
  if not (starts_with ~prefix:"---\n" content) then (None, content)
  else
    let rec find_closing_delimiter line_start =
      match String.index_from_opt content line_start '\n' with
      | None ->
        if String.sub content line_start (length - line_start) = "---" then
          Some (line_start, length)
        else None
      | Some newline_index ->
        let line = String.sub content line_start (newline_index - line_start) in
        if line = "---" then Some (line_start, newline_index + 1)
        else find_closing_delimiter (newline_index + 1)
    in
    match find_closing_delimiter 4 with
    | None -> (None, content)
    | Some (closing_start, body_start) ->
      let frontmatter_text = String.sub content 4 (closing_start - 4) in
      let body = String.sub content body_start (length - body_start) in
      (Some (parse_fields frontmatter_text), body)

(* --- accessors (additive, order-preserving) --- *)

let get (frontmatter : t) key = List.assoc_opt key frontmatter

let get_scalar frontmatter key =
  match get frontmatter key with Some (Scalar text) -> Some text | _ -> None

let mem frontmatter key = List.mem_assoc key frontmatter

let set (frontmatter : t) key value : t =
  if List.mem_assoc key frontmatter then
    List.map (fun (existing_key, existing_value) ->
        if existing_key = key then (existing_key, value) else (existing_key, existing_value))
      frontmatter
  else frontmatter @ [ (key, value) ]

(* set only if absent - the additive backfill primitive *)
let set_default (frontmatter : t) key value : t =
  if mem frontmatter key then frontmatter else frontmatter @ [ (key, value) ]

(* --- emit --- *)

let canonical_order =
  [ "id"; "type"; "owner"; "status"; "summary"; "updated"; "tags"; "related"; "sources";
    "last_verified" ]

let order_index key =
  let rec find position = function
    | [] -> max_int
    | candidate :: _ when candidate = key -> position
    | _ :: rest -> find (position + 1) rest
  in
  find 0 canonical_order

(* stable sort: canonical keys first in fixed order, the rest keep input order *)
let sort_canonical (frontmatter : t) : t =
  List.stable_sort
    (fun (left_key, _) (right_key, _) -> compare (order_index left_key) (order_index right_key))
    frontmatter

let needs_quoting text =
  text = ""
  ||
  let special_first_character =
    match text.[0] with
    | '-' | '?' | ':' | ',' | '[' | ']' | '{' | '}' | '#' | '&' | '*' | '!' | '|' | '>'
    | '\'' | '"' | '%' | '@' | '`' | ' ' ->
      true
    | _ -> false
  in
  special_first_character
  || text.[String.length text - 1] = ' '
  || String.exists (fun character -> character = '\n' || character = '\t' || Char.code character < 0x20) text
  ||
  let length = String.length text in
  let rec scan index =
    if index >= length - 1 then false
    else if text.[index] = ':' && text.[index + 1] = ' ' then true
    else if text.[index] = ' ' && text.[index + 1] = '#' then true
    else scan (index + 1)
  in
  scan 0

let quote text =
  let buffer = Buffer.create (String.length text + 2) in
  Buffer.add_char buffer '"';
  String.iter
    (fun character ->
      match character with
      | '"' -> Buffer.add_string buffer "\\\""
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '\n' -> Buffer.add_string buffer "\\n"
      | '\t' -> Buffer.add_string buffer "\\t"
      | other -> Buffer.add_char buffer other)
    text;
  Buffer.add_char buffer '"';
  Buffer.contents buffer

let emit_value text = if needs_quoting text then quote text else text

(* the bare frontmatter block, no delimiters, one canonical field per line *)
let to_block (frontmatter : t) : string =
  let buffer = Buffer.create 128 in
  List.iter
    (fun (key, value) ->
      match value with
      | Scalar text -> Buffer.add_string buffer (Printf.sprintf "%s: %s\n" key (emit_value text))
      | Items [] -> Buffer.add_string buffer (Printf.sprintf "%s: []\n" key)
      | Items items ->
        let rendered = items |> List.map emit_value |> String.concat ", " in
        Buffer.add_string buffer (Printf.sprintf "%s: [%s]\n" key rendered))
    (sort_canonical frontmatter);
  Buffer.contents buffer

let to_frontmatter (frontmatter : t) : string = "---\n" ^ to_block frontmatter ^ "---\n"

(* combine frontmatter with an untouched body into final file content *)
let render (frontmatter : t) (body : string) : string = to_frontmatter frontmatter ^ body
