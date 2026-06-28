(* The scaffold-time configuration (mcs.config.json). Defaults keep mcs usable in
   a directory that has no config yet, so [validate]/[index] work out of the box. *)

type link_style =
  | Wikilink
  | Markdown
  | Hybrid

type t =
  { link_style : link_style;
    areas : string list;
    index_path : string;
    index_moc_path : string;
    log_path : string;
    ignore : string list
  }

let link_style_of_string = function
  | "wikilink" -> Some Wikilink
  | "markdown" -> Some Markdown
  | "hybrid" -> Some Hybrid
  | _ -> None

let string_of_link_style = function
  | Wikilink -> "wikilink"
  | Markdown -> "markdown"
  | Hybrid -> "hybrid"

let default =
  { link_style = Hybrid;
    areas = [ "projects"; "areas"; "resources"; "archive" ];
    index_path = "index.json";
    index_moc_path = "index.md";
    log_path = "log.md";
    ignore = [ ".git"; "_opam"; "_build"; "node_modules"; ".obsidian" ]
  }

let of_json (json : Json.t) : t =
  let link_style =
    match Json.member "linkStyle" json with
    | Some (Json.String text) -> (
      match link_style_of_string text with Some style -> style | None -> default.link_style)
    | _ -> default.link_style
  in
  let areas =
    match Json.member "areas" json with
    | Some (Json.List _ as list) -> Json.string_list list
    | _ -> default.areas
  in
  let paths = match Json.member "paths" json with Some value -> value | None -> Json.Object [] in
  let path_member key fallback =
    match Json.member key paths with Some (Json.String text) -> text | _ -> fallback
  in
  let ignore =
    match Json.member "ignore" json with
    | Some (Json.List _ as list) -> Json.string_list list
    | _ -> default.ignore
  in
  { link_style;
    areas;
    index_path = path_member "index" default.index_path;
    index_moc_path = path_member "indexMoc" default.index_moc_path;
    log_path = path_member "log" default.log_path;
    ignore
  }

let load (path : string) : t =
  if Sys.file_exists path then (
    let input_channel = open_in_bin path in
    let length = in_channel_length input_channel in
    let content = really_input_string input_channel length in
    close_in input_channel;
    try of_json (Json.of_string content) with Json.Parse_error _ -> default)
  else default
