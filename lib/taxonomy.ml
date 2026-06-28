(* The type taxonomy as exhaustive variant types.

   Every `type` value traces to a published standard:
   - Diataxis        -> Tutorial / How_to / Reference / Explanation
   - MADR            -> Decision
   - Zettelkasten    -> Note
   - llm-wiki        -> Entity / Concept / Index / Log

   Because conversions and field rules below are exhaustive pattern matches,
   adding a constructor forces every site to handle it (correct-by-construction):
   the build fails until the new type is wired through everywhere. *)

type doc_type =
  | Tutorial
  | How_to
  | Reference
  | Explanation
  | Decision
  | Note
  | Entity
  | Concept
  | Index
  | Log

type owner =
  | Human
  | Agent
  | Shared

type status =
  | Draft
  | Current
  | Proposed
  | Accepted
  | Superseded
  | Deprecated

(* Enumerations - kept beside the variants so a new constructor surfaces here too. *)
let all_doc_types =
  [ Tutorial; How_to; Reference; Explanation; Decision; Note; Entity; Concept; Index; Log ]

let all_owners = [ Human; Agent; Shared ]

let all_statuses = [ Draft; Current; Proposed; Accepted; Superseded; Deprecated ]

(* --- doc_type <-> string --- *)

let string_of_doc_type = function
  | Tutorial -> "tutorial"
  | How_to -> "how-to"
  | Reference -> "reference"
  | Explanation -> "explanation"
  | Decision -> "decision"
  | Note -> "note"
  | Entity -> "entity"
  | Concept -> "concept"
  | Index -> "index"
  | Log -> "log"

let doc_type_of_string = function
  | "tutorial" -> Some Tutorial
  | "how-to" -> Some How_to
  | "reference" -> Some Reference
  | "explanation" -> Some Explanation
  | "decision" -> Some Decision
  | "note" -> Some Note
  | "entity" -> Some Entity
  | "concept" -> Some Concept
  | "index" -> Some Index
  | "log" -> Some Log
  | _ -> None

(* --- owner <-> string --- *)

let string_of_owner = function Human -> "human" | Agent -> "agent" | Shared -> "shared"

let owner_of_string = function
  | "human" -> Some Human
  | "agent" -> Some Agent
  | "shared" -> Some Shared
  | _ -> None

(* --- status <-> string --- *)

let string_of_status = function
  | Draft -> "draft"
  | Current -> "current"
  | Proposed -> "proposed"
  | Accepted -> "accepted"
  | Superseded -> "superseded"
  | Deprecated -> "deprecated"

let status_of_string = function
  | "draft" -> Some Draft
  | "current" -> Some Current
  | "proposed" -> Some Proposed
  | "accepted" -> Some Accepted
  | "superseded" -> Some Superseded
  | "deprecated" -> Some Deprecated
  | _ -> None

(* The lean-required frontmatter keys, enforced after a reconcile pass.
   Exhaustive over doc_type so introducing a type is a deliberate decision
   about its required fields rather than a silent default. v0.1: every type
   shares the same lean set (PRD section 8). *)
let required_fields (doc_type : doc_type) : string list =
  let lean_required = [ "id"; "type"; "owner"; "status"; "summary"; "updated" ] in
  match doc_type with
  | Tutorial | How_to | Reference | Explanation | Decision | Note | Entity | Concept
  | Index | Log ->
    lean_required

(* The default owner a reconcile pass assigns when backfilling a note of this
   type. Hand-capture genres default to human; the synthesis layer to agent. *)
let default_owner (doc_type : doc_type) : owner =
  match doc_type with
  | Tutorial | How_to | Reference | Explanation | Decision | Note -> Human
  | Entity | Concept | Index | Log -> Agent

(* The default status a reconcile pass assigns when backfilling. *)
let default_status (doc_type : doc_type) : status =
  match doc_type with
  | Decision -> Proposed
  | Tutorial | How_to | Reference | Explanation | Note | Entity | Concept | Index | Log ->
    Draft
