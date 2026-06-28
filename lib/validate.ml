(* Schema check of the required frontmatter set. This is the engine the pre-commit
   hook runs (writes nothing). A note with no frontmatter is "not yet reconciled",
   which is a distinct outcome from a note whose frontmatter is malformed. *)

type result =
  | Not_reconciled
  | Valid
  | Invalid of string list

let is_lowercase_digit_or_hyphen text =
  text <> ""
  && String.for_all (function 'a' .. 'z' | '0' .. '9' | '-' -> true | _ -> false) text

let valid_id_format identifier =
  is_lowercase_digit_or_hyphen identifier
  && identifier.[0] <> '-'
  && identifier.[String.length identifier - 1] <> '-'
  &&
  let rec no_double_hyphen index =
    index + 1 >= String.length identifier
    || ((not (identifier.[index] = '-' && identifier.[index + 1] = '-'))
       && no_double_hyphen (index + 1))
  in
  no_double_hyphen 0

let valid_date date =
  String.length date = 10
  && date.[4] = '-'
  && date.[7] = '-'
  &&
  let all_digits valid_so_far offset width =
    valid_so_far
    && String.for_all (fun character -> character >= '0' && character <= '9')
         (String.sub date offset width)
  in
  let digits_ok = all_digits true 0 4 in
  let digits_ok = all_digits digits_ok 5 2 in
  let digits_ok = all_digits digits_ok 8 2 in
  digits_ok
  &&
  let month = int_of_string (String.sub date 5 2) in
  let day = int_of_string (String.sub date 8 2) in
  month >= 1 && month <= 12 && day >= 1 && day <= 31

let lean_required = [ "id"; "type"; "owner"; "status"; "summary"; "updated" ]

let check (frontmatter : Frontmatter.t option) : result =
  match frontmatter with
  | None -> Not_reconciled
  | Some frontmatter ->
    let errors = ref [] in
    let report message = errors := message :: !errors in
    (* Required keys: type-specific when type is known and valid, else the lean set. *)
    let doc_type =
      match Frontmatter.get_scalar frontmatter "type" with
      | Some text -> Taxonomy.doc_type_of_string text
      | None -> None
    in
    let required =
      match doc_type with
      | Some doc_type -> Taxonomy.required_fields doc_type
      | None -> lean_required
    in
    List.iter
      (fun key ->
        match Frontmatter.get frontmatter key with
        | None -> report (Printf.sprintf "missing required field: %s" key)
        | Some (Frontmatter.Scalar "") -> report (Printf.sprintf "empty required field: %s" key)
        | Some _ -> ())
      required;
    (* Enumerated values must be drawn from the taxonomy. *)
    (match Frontmatter.get_scalar frontmatter "type" with
    | Some text when Taxonomy.doc_type_of_string text = None ->
      report (Printf.sprintf "unknown type: %s" text)
    | _ -> ());
    (match Frontmatter.get_scalar frontmatter "owner" with
    | Some text when Taxonomy.owner_of_string text = None ->
      report (Printf.sprintf "unknown owner: %s" text)
    | _ -> ());
    (match Frontmatter.get_scalar frontmatter "status" with
    | Some text when Taxonomy.status_of_string text = None ->
      report (Printf.sprintf "unknown status: %s" text)
    | _ -> ());
    (match Frontmatter.get_scalar frontmatter "id" with
    | Some text when not (valid_id_format text) -> report (Printf.sprintf "malformed id: %s" text)
    | _ -> ());
    (match Frontmatter.get_scalar frontmatter "updated" with
    | Some text when not (valid_date text) ->
      report (Printf.sprintf "malformed updated date: %s" text)
    | _ -> ());
    (match !errors with [] -> Valid | messages -> Invalid (List.rev messages))

let check_content (content : string) : result =
  let frontmatter, _body = Frontmatter.parse content in
  check frontmatter
