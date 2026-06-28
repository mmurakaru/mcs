(* Generate the machine index (index.json) and the human MOC (index.md).

   index.json is the portable backbone: a stable id -> path map plus the typed
   metadata, so agents never re-derive structure and link conversion is lossless.
   The note->entry step is pure (entries_of_notes), so it is unit-testable without
   touching the filesystem. *)

type entry =
  { id : string;
    path : string;
    doc_type : string;
    owner : string;
    status : string;
    summary : string;
    updated : string;
    title : string;
    tags : string list;
    related : string list
  }

let title_of ~path frontmatter body =
  match Reconcile.first_heading_title body with
  | Some heading -> heading
  | None -> (
    match frontmatter with
    | Some frontmatter -> (
      match Frontmatter.get_scalar frontmatter "summary" with
      | Some summary when summary <> "" -> summary
      | _ -> Reconcile.basename_without_extension path)
    | None -> Reconcile.basename_without_extension path)

let string_list_field frontmatter key =
  match Frontmatter.get frontmatter key with
  | Some (Frontmatter.Items items) -> items
  | Some (Frontmatter.Scalar text) when text <> "" -> [ text ]
  | _ -> []

(* Build an entry from a single note. Returns None for notes with no frontmatter
   or no id (not yet reconciled) - they don't belong in the index yet. *)
let entry_of_note ~path ~content : entry option =
  let frontmatter_option, body = Frontmatter.parse content in
  match frontmatter_option with
  | None -> None
  | Some frontmatter -> (
    match Frontmatter.get_scalar frontmatter "id" with
    | None | Some "" -> None
    | Some id ->
      let scalar key default =
        match Frontmatter.get_scalar frontmatter key with Some text -> text | None -> default
      in
      Some
        { id;
          path;
          doc_type = scalar "type" "";
          owner = scalar "owner" "";
          status = scalar "status" "";
          summary = scalar "summary" "";
          updated = scalar "updated" "";
          title = title_of ~path (Some frontmatter) body;
          tags = string_list_field frontmatter "tags";
          related = string_list_field frontmatter "related"
        })

let entries_of_notes (notes : (string * string) list) : entry list =
  notes
  |> List.filter_map (fun (path, content) -> entry_of_note ~path ~content)
  |> List.sort (fun left right -> compare left.id right.id)

(* --- emit index.json --- *)

let json_of_entry entry : Json.t =
  Json.Object
    [ ("id", Json.String entry.id);
      ("path", Json.String entry.path);
      ("type", Json.String entry.doc_type);
      ("owner", Json.String entry.owner);
      ("status", Json.String entry.status);
      ("title", Json.String entry.title);
      ("summary", Json.String entry.summary);
      ("updated", Json.String entry.updated);
      ("tags", Json.List (List.map (fun tag -> Json.String tag) entry.tags));
      ("related", Json.List (List.map (fun related_id -> Json.String related_id) entry.related))
    ]

let to_json (entries : entry list) : Json.t =
  Json.Object
    [ ("version", Json.Number 1.);
      ("count", Json.Number (float_of_int (List.length entries)));
      ("notes", Json.List (List.map json_of_entry entries))
    ]

(* --- emit index.md (human MOC, grouped by type in taxonomy order) --- *)

let to_moc (entries : entry list) : string =
  let buffer = Buffer.create 1024 in
  Buffer.add_string buffer "# Index\n\n";
  Buffer.add_string buffer
    (Printf.sprintf "Machine-generated map of content. %d typed notes.\n"
       (List.length entries));
  List.iter
    (fun doc_type ->
      let type_string = Taxonomy.string_of_doc_type doc_type in
      let group = List.filter (fun entry -> entry.doc_type = type_string) entries in
      if group <> [] then (
        Buffer.add_string buffer (Printf.sprintf "\n## %s\n\n" type_string);
        List.iter
          (fun entry ->
            Buffer.add_string buffer
              (Printf.sprintf "- [%s](%s) - %s\n" entry.title entry.path entry.summary))
          group))
    Taxonomy.all_doc_types;
  Buffer.contents buffer

(* --- filesystem walk --- *)

let rec walk ~ignore root relative accumulator =
  let directory = if relative = "" then root else Filename.concat root relative in
  let names = try Sys.readdir directory with Sys_error _ -> [||] in
  Array.fold_left
    (fun accumulator name ->
      if List.mem name ignore then accumulator
      else
        let relative_path = if relative = "" then name else Filename.concat relative name in
        let full_path = Filename.concat root relative_path in
        if Sys.is_directory full_path then walk ~ignore root relative_path accumulator
        else if Filename.check_suffix name ".md" then relative_path :: accumulator
        else accumulator)
    accumulator names

let find_markdown_files ~(config : Config.t) ~root : string list =
  let generated = [ config.index_moc_path; config.log_path ] in
  walk ~ignore:config.ignore root "" []
  |> List.filter (fun path -> not (List.mem path generated))
  |> List.sort compare

let read_file path =
  let input_channel = open_in_bin path in
  let length = in_channel_length input_channel in
  let content = really_input_string input_channel length in
  close_in input_channel;
  content

let build ~(config : Config.t) ~root : entry list =
  find_markdown_files ~config ~root
  |> List.map (fun relative -> (relative, read_file (Filename.concat root relative)))
  |> entries_of_notes
