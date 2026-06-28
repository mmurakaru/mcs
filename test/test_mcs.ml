(* Dependency-free test harness: each [check] records pass/fail; the process
   exits non-zero if any assertion failed so [dune test] / CI reports red. *)

let failures = ref 0
let total = ref 0

let check name condition =
  incr total;
  if not condition then (
    incr failures;
    Printf.printf "FAIL: %s\n" name)

let expect_equal name actual expected =
  incr total;
  if actual <> expected then (
    incr failures;
    Printf.printf "FAIL: %s\n  expected: %S\n  actual:   %S\n" name expected actual)

(* small substring test used by the MOC assertions *)
let contains haystack needle =
  let needle_length = String.length needle in
  let rec search index =
    index + needle_length <= String.length haystack
    && (String.sub haystack index needle_length = needle || search (index + 1))
  in
  search 0

(* ---------- Slug ---------- *)

let test_slug () =
  expect_equal "slug lowercases and hyphenates" (Slug.slugify "Hello World") "hello-world";
  expect_equal "slug collapses punctuation runs" (Slug.slugify "A, B & C!!") "a-b-c";
  expect_equal "slug trims edges" (Slug.slugify "  spaced  ") "spaced";
  expect_equal "slug keeps digits" (Slug.slugify "Note 42 v2") "note-42-v2";
  expect_equal "slug empty stays empty" (Slug.slugify "!!!") ""

(* ---------- Json ---------- *)

let test_json () =
  let value =
    Json.Object
      [ ("a", Json.Number 1.); ("b", Json.String "x\"y");
        ("c", Json.List [ Json.Boolean true; Json.Null ]) ]
  in
  let compact = Json.to_string ~pretty:false value in
  expect_equal "json compact emit" compact {|{"a":1,"b":"x\"y","c":[true,null]}|};
  let round_tripped = Json.of_string (Json.to_string ~pretty:true value) in
  check "json round-trips structurally" (round_tripped = value);
  expect_equal "json escapes newline" (Json.to_string ~pretty:false (Json.String "a\nb")) {|"a\nb"|};
  (match Json.of_string {|  { "k" : [1, 2, 3] , "s":"hi" }  |} with
  | Json.Object _ -> check "json parses with whitespace" true
  | _ -> check "json parses with whitespace" false);
  expect_equal "json member"
    (match Json.member "s" (Json.of_string {|{"s":"hi"}|}) with
    | Some (Json.String text) -> text
    | _ -> "?")
    "hi"

(* ---------- Taxonomy ---------- *)

let test_taxonomy () =
  expect_equal "doc_type count" (string_of_int (List.length Taxonomy.all_doc_types)) "10";
  List.iter
    (fun doc_type ->
      let text = Taxonomy.string_of_doc_type doc_type in
      check ("doc_type round-trips: " ^ text) (Taxonomy.doc_type_of_string text = Some doc_type))
    Taxonomy.all_doc_types;
  List.iter
    (fun owner ->
      check "owner round-trips" (Taxonomy.owner_of_string (Taxonomy.string_of_owner owner) = Some owner))
    Taxonomy.all_owners;
  List.iter
    (fun status ->
      check "status round-trips"
        (Taxonomy.status_of_string (Taxonomy.string_of_status status) = Some status))
    Taxonomy.all_statuses;
  check "unknown doc_type is None" (Taxonomy.doc_type_of_string "widget" = None);
  expect_equal "how-to spelling" (Taxonomy.string_of_doc_type Taxonomy.How_to) "how-to";
  check "decision defaults to proposed" (Taxonomy.default_status Taxonomy.Decision = Taxonomy.Proposed);
  check "note defaults to human owner" (Taxonomy.default_owner Taxonomy.Note = Taxonomy.Human);
  check "concept defaults to agent owner" (Taxonomy.default_owner Taxonomy.Concept = Taxonomy.Agent)

(* ---------- Frontmatter ---------- *)

let sample =
  "---\n\
   id: projects-note-foo\n\
   type: note\n\
   tags: [a, b]\n\
   ---\n\
   # Foo\n\nBody text.\n"

let scalar frontmatter key = Option.value ~default:"" (Frontmatter.get_scalar frontmatter key)

let test_frontmatter () =
  let frontmatter_option, body = Frontmatter.parse sample in
  (match frontmatter_option with
  | None -> check "frontmatter present" false
  | Some frontmatter ->
    check "parsed id" (Frontmatter.get_scalar frontmatter "id" = Some "projects-note-foo");
    check "parsed inline list"
      (Frontmatter.get frontmatter "tags" = Some (Frontmatter.Items [ "a"; "b" ])));
  expect_equal "body is byte-exact" body "# Foo\n\nBody text.\n";
  (* no frontmatter -> body is whole content *)
  let no_frontmatter, whole_body = Frontmatter.parse "# Just a title\n" in
  check "no frontmatter detected" (no_frontmatter = None);
  expect_equal "whole content is body" whole_body "# Just a title\n";
  (* block list parse *)
  let block_frontmatter, _ = Frontmatter.parse "---\nrelated:\n  - x\n  - y\n---\nbody" in
  (match block_frontmatter with
  | Some frontmatter ->
    check "block list parsed"
      (Frontmatter.get frontmatter "related" = Some (Frontmatter.Items [ "x"; "y" ]))
  | None -> check "block list parsed" false);
  (* emit: canonical order + quoting *)
  let fields =
    [ ("summary", Frontmatter.Scalar "has: colon"); ("id", Frontmatter.Scalar "a-note-b");
      ("type", Frontmatter.Scalar "note") ]
  in
  let rendered = Frontmatter.to_block fields in
  expect_equal "emit canonical order + quote colon" rendered
    "id: a-note-b\ntype: note\nsummary: \"has: colon\"\n";
  (* render reattaches body untouched *)
  expect_equal "render reattaches body"
    (Frontmatter.render [ ("id", Frontmatter.Scalar "x") ] "BODY") "---\nid: x\n---\nBODY"

(* ---------- Validate ---------- *)

let test_validate () =
  check "no frontmatter is not_reconciled"
    (Validate.check_content "# hi\n" = Validate.Not_reconciled);
  let valid_note =
    "---\nid: projects-note-foo\ntype: note\nowner: human\nstatus: draft\nsummary: hi\nupdated: 2026-06-28\n---\nbody"
  in
  check "complete note is valid" (Validate.check_content valid_note = Validate.Valid);
  (match
     Validate.check_content
       "---\ntype: note\nowner: human\nstatus: draft\nsummary: hi\nupdated: 2026-06-28\n---\nb"
   with
  | Validate.Invalid errors ->
    check "missing id reported"
      (List.exists (fun error -> String.length error >= 7 && String.sub error 0 7 = "missing") errors)
  | _ -> check "missing id reported" false);
  (match
     Validate.check_content
       "---\nid: projects-note-foo\ntype: widget\nowner: human\nstatus: draft\nsummary: hi\nupdated: 2026-06-28\n---\nb"
   with
  | Validate.Invalid _ -> check "unknown type rejected" true
  | _ -> check "unknown type rejected" false);
  check "date validator" (Validate.valid_date "2026-06-28");
  check "bad date rejected" (not (Validate.valid_date "2026-13-99"));
  check "bad date format rejected" (not (Validate.valid_date "2026/06/28"));
  check "id validator" (Validate.valid_id_format "projects-note-foo");
  check "double hyphen id rejected" (not (Validate.valid_id_format "a--b"));
  check "trailing hyphen id rejected" (not (Validate.valid_id_format "a-b-"))

(* ---------- Reconcile ---------- *)

let test_reconcile () =
  let note = "# My Title\n\nThis is the first prose line.\n\nMore.\n" in
  let reconciled =
    Reconcile.reconcile_content ~today:"2026-06-28" ~area:"projects" ~name_hint:"my-title" note
  in
  let frontmatter_option, body = Frontmatter.parse reconciled in
  expect_equal "body byte-identical after reconcile" body note;
  (match frontmatter_option with
  | None -> check "frontmatter added" false
  | Some frontmatter ->
    expect_equal "id derived from area+type+title" (scalar frontmatter "id") "projects-note-my-title";
    expect_equal "type defaults to note" (scalar frontmatter "type") "note";
    expect_equal "owner defaults to human" (scalar frontmatter "owner") "human";
    expect_equal "summary from first prose" (scalar frontmatter "summary") "This is the first prose line.";
    expect_equal "updated set to today" (scalar frontmatter "updated") "2026-06-28");
  (* idempotency *)
  let second_pass =
    Reconcile.reconcile_content ~today:"2099-01-01" ~area:"projects" ~name_hint:"my-title" reconciled
  in
  expect_equal "reconcile is idempotent" second_pass reconciled;
  (* additive: existing values preserved *)
  let typed_note = "---\ntype: decision\nowner: shared\n---\n# Pick X\n\nWe choose X.\n" in
  let typed_reconciled =
    Reconcile.reconcile_content ~today:"2026-06-28" ~area:"areas" ~name_hint:"pick-x" typed_note
  in
  let typed_frontmatter, _ = Frontmatter.parse typed_reconciled in
  (match typed_frontmatter with
  | Some frontmatter ->
    expect_equal "existing type preserved" (scalar frontmatter "type") "decision";
    expect_equal "existing owner preserved" (scalar frontmatter "owner") "shared";
    expect_equal "id uses existing type" (scalar frontmatter "id") "areas-decision-pick-x";
    expect_equal "status from type default" (scalar frontmatter "status") "proposed"
  | None -> check "typed reconcile" false);
  (* scaffold path: a seeded type must drive the id + status defaults *)
  let seed_frontmatter =
    [ ("type", Frontmatter.Scalar "decision"); ("summary", Frontmatter.Scalar "T") ]
  in
  let scaffolded =
    Reconcile.backfill ~today:"2026-06-28" ~area:"notes" ~name_hint:"adopt-x" (Some seed_frontmatter)
      "# Adopt X\n\nbody\n"
  in
  expect_equal "seed: id uses seeded type" (scalar scaffolded "id") "notes-decision-adopt-x";
  expect_equal "seed: status from decision default" (scalar scaffolded "status") "proposed";
  expect_equal "seed: summary preserved" (scalar scaffolded "summary") "T"

let test_links () =
  let resolve = function
    | "Foo Note" -> Some "notes/foo.md"
    | "bar" -> Some "bar.md"
    | _ -> None
  in
  expect_equal "wikilink -> markdown with alias"
    (Reconcile.convert_to_markdown ~resolve "see [[Foo Note|the foo]] here")
    "see [the foo](notes/foo.md) here";
  expect_equal "wikilink -> markdown bare"
    (Reconcile.convert_to_markdown ~resolve "see [[bar]] ok")
    "see [bar](bar.md) ok";
  expect_equal "unresolved wikilink untouched"
    (Reconcile.convert_to_markdown ~resolve "see [[unknown]] ok")
    "see [[unknown]] ok";
  expect_equal "markdown -> wikilink"
    (Reconcile.convert_to_wikilink "see [the foo](notes/foo.md) ok")
    "see [[foo|the foo]] ok";
  expect_equal "external link untouched"
    (Reconcile.convert_to_wikilink "see [site](https://x.com) ok")
    "see [site](https://x.com) ok"

(* ---------- Index ---------- *)

let test_index () =
  let notes =
    [ ( "notes/foo.md",
        "---\nid: projects-note-foo\ntype: note\nowner: human\nstatus: draft\nsummary: A foo.\nupdated: 2026-06-28\ntags: [x]\n---\n# Foo\n" );
      ( "notes/dec.md",
        "---\nid: areas-decision-pick\ntype: decision\nowner: shared\nstatus: accepted\nsummary: We pick.\nupdated: 2026-06-28\n---\n# Pick\n" );
      ("draft.md", "# No frontmatter yet\n") ]
  in
  let entries = Index.entries_of_notes notes in
  expect_equal "two reconciled notes indexed" (string_of_int (List.length entries)) "2";
  let json = Index.to_json entries in
  check "index json has count 2" (Json.member "count" json = Some (Json.Number 2.));
  let moc = Index.to_moc entries in
  check "moc groups note type" (contains moc "## note");
  check "moc links by path" (contains moc "(notes/foo.md)")

let () =
  test_slug ();
  test_json ();
  test_taxonomy ();
  test_frontmatter ();
  test_validate ();
  test_reconcile ();
  test_links ();
  test_index ();
  if !failures = 0 then Printf.printf "OK: %d checks passed\n" !total
  else (
    Printf.printf "\n%d/%d checks FAILED\n" !failures !total;
    exit 1)
