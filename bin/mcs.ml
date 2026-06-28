(* mcs CLI: validate | index | scaffold | reconcile.

   reconcile orchestrates the additive backfill + index rebuild. validate is the
   read-only engine the pre-commit hook runs. Library code stays pure; this entry
   point owns the side effects (clock, filesystem, exit codes). *)

let today () =
  let local_time = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02d" (local_time.Unix.tm_year + 1900) (local_time.Unix.tm_mon + 1)
    local_time.Unix.tm_mday

let read_file path =
  let input_channel = open_in_bin path in
  let length = in_channel_length input_channel in
  let content = really_input_string input_channel length in
  close_in input_channel;
  content

let write_file path content =
  let output_channel = open_out_bin path in
  output_string output_channel content;
  close_out output_channel

(* replace every occurrence of [needle] in [text] with [replacement] *)
let replace_all text needle replacement =
  if needle = "" then text
  else
    let buffer = Buffer.create (String.length text) in
    let needle_length = String.length needle in
    let index = ref 0 in
    let length = String.length text in
    while !index < length do
      if !index + needle_length <= length && String.sub text !index needle_length = needle then (
        Buffer.add_string buffer replacement;
        index := !index + needle_length)
      else (
        Buffer.add_char buffer text.[!index];
        incr index)
    done;
    Buffer.contents buffer

let rec make_directories directory =
  if directory = "" || directory = "." || Sys.file_exists directory then ()
  else (
    make_directories (Filename.dirname directory);
    try Unix.mkdir directory 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let load_config root = Config.load (Filename.concat root "mcs.config.json")

(* relative path from the directory of [from_path] to [to_path], both root-relative *)
let relative_path ~from_path ~to_path =
  let split path = String.split_on_char '/' path |> List.filter (fun segment -> segment <> "") in
  let from_directory =
    let directory = Filename.dirname from_path in
    if directory = "." then [] else split directory
  in
  let to_segments = split to_path in
  let rec drop_common left right =
    match (left, right) with
    | first_left :: rest_left, first_right :: rest_right when first_left = first_right ->
      drop_common rest_left rest_right
    | _ -> (left, right)
  in
  let up_segments, down_segments = drop_common from_directory to_segments in
  let parent_steps = List.map (fun _ -> "..") up_segments in
  match parent_steps @ down_segments with
  | [] -> Filename.basename to_path
  | segments -> String.concat "/" segments

(* ---------- subcommands ---------- *)

let run_validate root =
  let config = load_config root in
  let files = Index.find_markdown_files ~config ~root in
  let invalid = ref 0 and reconciled = ref 0 and pending = ref 0 in
  List.iter
    (fun relative_path ->
      let content = read_file (Filename.concat root relative_path) in
      match Validate.check_content content with
      | Validate.Valid -> incr reconciled
      | Validate.Not_reconciled -> incr pending
      | Validate.Invalid errors ->
        incr invalid;
        Printf.eprintf "INVALID %s\n" relative_path;
        List.iter (fun error -> Printf.eprintf "  - %s\n" error) errors)
    files;
  Printf.printf "validated %d files: %d valid, %d not-yet-reconciled, %d invalid\n"
    (List.length files) !reconciled !pending !invalid;
  if !invalid > 0 then exit 1

let write_index root config entries =
  let json = Index.to_json entries in
  write_file (Filename.concat root config.Config.index_path)
    (Json.to_string ~pretty:true json ^ "\n");
  write_file (Filename.concat root config.Config.index_moc_path) (Index.to_moc entries)

(* Ids must be unique (the index is an id -> path map). Report collisions and bail
   before writing a corrupt index; the fix is an explicit `id:` on one of the notes. *)
let ensure_unique_ids entries =
  match Index.find_duplicate_ids entries with
  | [] -> ()
  | duplicates ->
    Printf.eprintf "duplicate ids (set an explicit `id:` on one of each set):\n";
    List.iter
      (fun (id, paths) ->
        Printf.eprintf "  %s\n" id;
        List.iter (fun path -> Printf.eprintf "    - %s\n" path) paths)
      duplicates;
    exit 1

let run_index root =
  let config = load_config root in
  let entries = Index.build ~config ~root in
  ensure_unique_ids entries;
  write_index root config entries;
  Printf.printf "indexed %d notes -> %s + %s\n" (List.length entries) config.Config.index_path
    config.Config.index_moc_path

let run_scaffold arguments =
  match arguments with
  | type_string :: title :: rest ->
    (match Taxonomy.doc_type_of_string type_string with
    | None ->
      Printf.eprintf "unknown type: %s\n" type_string;
      exit 2
    | Some _ ->
      let area = ref "notes" and output_path = ref None in
      let rec parse = function
        | "--area" :: value :: rest ->
          area := value;
          parse rest
        | "--out" :: value :: rest ->
          output_path := Some value;
          parse rest
        | [] -> ()
        | other :: rest ->
          Printf.eprintf "ignoring argument: %s\n" other;
          parse rest
      in
      parse rest;
      let slug = Slug.slugify title in
      let template_path = Filename.concat "doc-templates" (type_string ^ ".md") in
      let body =
        if Sys.file_exists template_path then
          let template_text = read_file template_path in
          replace_all template_text "{{title}}" title
        else Printf.sprintf "# %s\n\n" title
      in
      (* seed the chosen type + a title summary so the backfill respects them
         rather than defaulting type to `note` and summarizing template filler *)
      let seed_frontmatter =
        [ ("type", Frontmatter.Scalar type_string); ("summary", Frontmatter.Scalar title) ]
      in
      let frontmatter =
        Reconcile.backfill ~today:(today ()) ~area:!area ~slug (Some seed_frontmatter) body
      in
      let content = Frontmatter.render frontmatter body in
      let path =
        match !output_path with Some path -> path | None -> Filename.concat !area (slug ^ ".md")
      in
      make_directories (Filename.dirname path);
      if Sys.file_exists path then (
        Printf.eprintf "refusing to overwrite existing file: %s\n" path;
        exit 1);
      write_file path content;
      Printf.printf "%s\n" path)
  | _ ->
    Printf.eprintf "usage: mcs scaffold <type> \"<title>\" [--area AREA] [--out PATH]\n";
    exit 2

(* build a resolver (wikilink target -> relative path) from current notes *)
let build_resolver root files ~from_path =
  let lookup = Hashtbl.create 64 in
  List.iter
    (fun relative_path ->
      let content = read_file (Filename.concat root relative_path) in
      let _, body = Frontmatter.parse content in
      Hashtbl.replace lookup (Reconcile.basename_without_extension relative_path) relative_path;
      match Reconcile.first_heading_title body with
      | Some title -> Hashtbl.replace lookup title relative_path
      | None -> ())
    files;
  fun target ->
    match Hashtbl.find_opt lookup target with
    | Some to_path -> Some (relative_path ~from_path ~to_path)
    | None -> None

let run_reconcile root convert_links =
  let config = load_config root in
  let files = Index.find_markdown_files ~config ~root in
  let today = today () in
  (* 1. additive frontmatter backfill, prose untouched *)
  let changed_count = ref 0 in
  List.iter
    (fun relative_path ->
      let full_path = Filename.concat root relative_path in
      let content = read_file full_path in
      let reconciled = Reconcile.reconcile_content ~today ~relative_path content in
      if reconciled <> content then (
        write_file full_path reconciled;
        incr changed_count))
    files;
  (* 2. optional link-style conversion *)
  (match convert_links with
  | None -> ()
  | Some style ->
    let converted_count = ref 0 in
    List.iter
      (fun relative_path ->
        let full_path = Filename.concat root relative_path in
        let content = read_file full_path in
        let frontmatter_option, body = Frontmatter.parse content in
        let resolve = build_resolver root files ~from_path:relative_path in
        let converted_body =
          match style with
          | Config.Markdown -> Reconcile.convert_to_markdown ~resolve body
          | Config.Wikilink -> Reconcile.convert_to_wikilink body
          | Config.Hybrid -> body
        in
        if converted_body <> body then (
          let output =
            match frontmatter_option with
            | Some frontmatter -> Frontmatter.render frontmatter converted_body
            | None -> converted_body
          in
          write_file full_path output;
          incr converted_count))
      files;
    Printf.printf "converted links in %d files -> %s\n" !converted_count
      (Config.string_of_link_style style));
  (* 3. rebuild index from the reconciled tree *)
  let entries = Index.build ~config ~root in
  ensure_unique_ids entries;
  write_index root config entries;
  Printf.printf "reconciled %d files (%d changed), indexed %d notes\n" (List.length files)
    !changed_count (List.length entries)

let usage () =
  print_string
    "mcs - a personal wiki bookkeeper\n\n\
     usage:\n\
    \  mcs validate [path]                       schema-check required frontmatter (no writes)\n\
    \  mcs index [path]                          rebuild index.json + _index.md\n\
    \  mcs scaffold <type> \"<title>\" [opts]      create a new typed note\n\
    \  mcs reconcile [path] [--convert-links S]  backfill frontmatter + rebuild index\n\n\
     types: tutorial how-to reference explanation decision note entity concept index log\n\
     link styles (S): wikilink | markdown | hybrid\n"

let () =
  match Array.to_list Sys.argv with
  | _ :: "validate" :: rest -> run_validate (match rest with path :: _ -> path | [] -> ".")
  | _ :: "index" :: rest -> run_index (match rest with path :: _ -> path | [] -> ".")
  | _ :: "scaffold" :: rest -> run_scaffold rest
  | _ :: "reconcile" :: rest ->
    let root = ref "." and convert = ref None in
    let rec parse = function
      | "--convert-links" :: value :: rest ->
        (match Config.link_style_of_string value with
        | Some style -> convert := Some style
        | None ->
          Printf.eprintf "unknown link style: %s\n" value;
          exit 2);
        parse rest
      | path :: rest when String.length path > 0 && path.[0] <> '-' ->
        root := path;
        parse rest
      | [] -> ()
      | other :: rest ->
        Printf.eprintf "ignoring argument: %s\n" other;
        parse rest
    in
    parse rest;
    run_reconcile !root !convert
  | _ :: ("--version" | "version") :: _ -> print_endline Version.value
  | _ :: ("--help" | "-h" | "help") :: _ -> usage ()
  | [ _ ] -> usage ()
  | _ ->
    usage ();
    exit 2
