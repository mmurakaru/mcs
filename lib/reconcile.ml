(* The bookkeeper pass, as pure transformations over a note's content.

   Guardrails baked into the types: backfill only ever calls set_default (additive
   - never overwrites a human value) and reconcile_content re-attaches the parsed
   body byte-for-byte. There is no code path here that shortens or rewrites prose. *)

(* --- deriving fields from the body --- *)

let lines body = String.split_on_char '\n' body

(* first ATX level-1 heading ("# Title"), with the marker stripped *)
let first_heading_title body =
  List.find_map
    (fun line ->
      let stripped = String.trim line in
      if String.length stripped >= 2 && stripped.[0] = '#' && stripped.[1] = ' ' then
        Some (String.trim (String.sub stripped 1 (String.length stripped - 1)))
      else None)
    (lines body)

(* first line of real prose - skips blanks, headings, and rule lines.
   Used to backfill [summary] when the human wrote none. *)
let first_prose_line body =
  List.find_map
    (fun line ->
      let stripped = String.trim line in
      if stripped = "" then None
      else if stripped.[0] = '#' then None
      else if stripped = "---" || stripped = "***" || stripped = "___" then None
      else Some stripped)
    (lines body)

let truncate_summary text =
  let maximum_length = 280 in
  if String.length text <= maximum_length then text else String.sub text 0 maximum_length

(* --- additive backfill --- *)

(* Resolve the doc type from existing frontmatter, defaulting to the
   Zettelkasten capture primitive when the human typed nothing. *)
let resolve_type frontmatter =
  match Frontmatter.get_scalar frontmatter "type" with
  | Some text -> (
    match Taxonomy.doc_type_of_string text with Some doc_type -> doc_type | None -> Taxonomy.Note)
  | None -> Taxonomy.Note

(* Backfill the lean-required set without ever overwriting an existing value.
   [area] seeds the id prefix; [name_hint] (e.g. the filename stem) is the slug
   source when the body has no level-1 heading. *)
let backfill ~today ~area ~name_hint (frontmatter_option : Frontmatter.t option) (body : string)
    : Frontmatter.t =
  let frontmatter = Option.value frontmatter_option ~default:[] in
  let doc_type = resolve_type frontmatter in
  let title = match first_heading_title body with Some heading -> heading | None -> name_hint in
  let slug = Slug.slugify title in
  let identifier =
    Printf.sprintf "%s-%s-%s" (Slug.slugify area) (Taxonomy.string_of_doc_type doc_type) slug
  in
  let summary =
    match first_prose_line body with Some line -> truncate_summary line | None -> title
  in
  let open Frontmatter in
  frontmatter
  |> (fun frontmatter -> set_default frontmatter "id" (Scalar identifier))
  |> (fun frontmatter -> set_default frontmatter "type" (Scalar (Taxonomy.string_of_doc_type doc_type)))
  |> (fun frontmatter ->
       set_default frontmatter "owner" (Scalar (Taxonomy.string_of_owner (Taxonomy.default_owner doc_type))))
  |> (fun frontmatter ->
       set_default frontmatter "status" (Scalar (Taxonomy.string_of_status (Taxonomy.default_status doc_type))))
  |> (fun frontmatter -> set_default frontmatter "summary" (Scalar summary))
  |> fun frontmatter -> set_default frontmatter "updated" (Scalar today)

(* Reconcile a whole file's content: parse, backfill, re-render. The body is the
   byte-exact substring returned by the parser, so prose is never mutated. A
   second pass over the result is a fixed point (idempotent). *)
let reconcile_content ~today ~area ~name_hint (content : string) : string =
  let frontmatter_option, body = Frontmatter.parse content in
  let frontmatter = backfill ~today ~area ~name_hint frontmatter_option body in
  Frontmatter.render frontmatter body

(* --- link-style conversion (portability) --- *)

(* Convert the body's link surface syntax. [resolve] maps a wikilink target
   (a note name, no extension) to a path relative to the file being converted;
   it returns None when the target cannot be resolved, in which case the link is
   left untouched so nothing silently breaks. *)

let basename_without_extension path =
  let base = Filename.basename path in
  match Filename.chop_suffix_opt ~suffix:".md" base with Some stem -> stem | None -> base

(* rewrite each "[[ ... ]]" wikilink into a "[alias](path)" markdown link *)
let convert_to_markdown ~resolve (body : string) : string =
  let buffer = Buffer.create (String.length body) in
  let length = String.length body in
  let index = ref 0 in
  while !index < length do
    if !index + 1 < length && body.[!index] = '[' && body.[!index + 1] = '[' then (
      match
        let rec find_close scan_index =
          if scan_index + 1 >= length then None
          else if body.[scan_index] = ']' && body.[scan_index + 1] = ']' then Some scan_index
          else find_close (scan_index + 1)
        in
        find_close (!index + 2)
      with
      | None ->
        Buffer.add_char buffer body.[!index];
        incr index
      | Some close_index ->
        let inner = String.sub body (!index + 2) (close_index - (!index + 2)) in
        (* inner may be "target|alias" or "target#anchor|alias" *)
        let target_part, alias =
          match String.index_opt inner '|' with
          | Some bar_index ->
            ( String.sub inner 0 bar_index,
              String.sub inner (bar_index + 1) (String.length inner - bar_index - 1) )
          | None -> (inner, inner)
        in
        let target =
          match String.index_opt target_part '#' with
          | Some anchor_index -> String.sub target_part 0 anchor_index
          | None -> target_part
        in
        (match resolve (String.trim target) with
        | Some path -> Buffer.add_string buffer (Printf.sprintf "[%s](%s)" alias path)
        | None -> Buffer.add_string buffer (String.sub body !index (close_index + 2 - !index)));
        index := close_index + 2)
    else (
      Buffer.add_char buffer body.[!index];
      incr index)
  done;
  Buffer.contents buffer

(* rewrite markdown links to local .md files into wikilinks *)
let convert_to_wikilink (body : string) : string =
  let buffer = Buffer.create (String.length body) in
  let length = String.length body in
  let index = ref 0 in
  while !index < length do
    if body.[!index] = '[' && not (!index + 1 < length && body.[!index + 1] = '[') then (
      match
        let rec find_text_close scan_index =
          if scan_index >= length then None
          else if body.[scan_index] = ']' then Some scan_index
          else if body.[scan_index] = '\n' then None
          else find_text_close (scan_index + 1)
        in
        find_text_close (!index + 1)
      with
      | Some text_close when text_close + 1 < length && body.[text_close + 1] = '(' ->
        let text = String.sub body (!index + 1) (text_close - (!index + 1)) in
        let rec find_paren_close scan_index =
          if scan_index >= length then None
          else if body.[scan_index] = ')' then Some scan_index
          else if body.[scan_index] = '\n' then None
          else find_paren_close (scan_index + 1)
        in
        (match find_paren_close (text_close + 2) with
        | Some paren_close ->
          let url = String.sub body (text_close + 2) (paren_close - (text_close + 2)) in
          let is_local_markdown =
            (not (String.length url >= 4 && String.sub url 0 4 = "http"))
            && Filename.check_suffix url ".md"
          in
          if is_local_markdown then (
            let stem = basename_without_extension url in
            if text = stem then Buffer.add_string buffer (Printf.sprintf "[[%s]]" stem)
            else Buffer.add_string buffer (Printf.sprintf "[[%s|%s]]" stem text);
            index := paren_close + 1)
          else (
            Buffer.add_char buffer body.[!index];
            incr index)
        | None ->
          Buffer.add_char buffer body.[!index];
          incr index)
      | _ ->
        Buffer.add_char buffer body.[!index];
        incr index)
    else (
      Buffer.add_char buffer body.[!index];
      incr index)
  done;
  Buffer.contents buffer
