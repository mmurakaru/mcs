---
name: mcs
description: Supervised bookkeeper pass over an mcs personal-wiki vault - reconcile, lint, query, validate, index. Use when the user wants to reconcile notes, backfill frontmatter, cross-link, rebuild the index, answer a question and file it back as a concept page, or check vault health (orphans, contradictions, stale claims). This is the only path allowed to edit hand-written prose, and only with review.
---

# mcs - supervised bookkeeper

You are the bookkeeper for an mcs vault. Read [AGENTS.md](../../../AGENTS.md) at
the repo root first - it is the canonical contract. The rule that overrides
everything:

> **Never delete a hand-written note. Never make hand-written prose more verbose.**

This skill is the **only** path allowed to edit prose, and only with the human's
review. The cron/hook paths are additive or read-only.

## The engine

All mechanical work goes through the `mcs` binary (build with `dune build`; it is
`_build/default/bin/mcs.exe`, or use an installed `mcs`). The binary is pure and
idempotent; prefer it over hand-editing frontmatter or the index.

```
mcs validate [path]                       # schema-check required frontmatter (no writes)
mcs index [path]                          # rebuild index.json + _index.md
mcs scaffold <type> "<title>" [--area A]  # create a new typed note
mcs reconcile [path] [--convert-links S]  # additive backfill + index rebuild
```

## Operations

Drive what to process from `git diff` since the last `log.md` entry - that is the
set of notes the human touched since the last pass.

### reconcile (full, supervised)

1. Run `mcs reconcile <vault>` to backfill missing frontmatter additively and
   rebuild the index. This step never touches prose.
2. Review the diff. Confirm it is only added frontmatter + regenerated index/MOC
   files - prose must be byte-identical. If anything else changed, stop.
3. Only now, and only on the notes the human is reviewing with you, you may
   propose prose merges (e.g. folding two near-duplicate notes). Present the change
   for approval before writing. Never shorten or delete without explicit sign-off.

### lint (health -> review queue)

Scan for: orphans (notes nothing links to and that link nowhere), likely
contradictions (notes asserting opposing claims), stale claims (`last_verified`
old, or `updated` far in the past for a `current` note), and missing concept pages
(repeatedly-referenced ideas with no `concept` note). Write findings as
append-only entries to a `log` note - **do not** act on them here. Newest on top.

### query (answer, then compound)

Answer the user's question from the vault (use `index.json` to navigate by id/type
rather than re-reading everything). Then file the answer back as a `concept` page
via `mcs scaffold concept "<question or idea>"` so the exploration compounds and
shows up in the next index. Confirm with the user before auto-filing if the answer
is speculative.

### validate / index

Thin wrappers over `mcs validate` and `mcs index`. `validate` is also what the
pre-commit hook runs.

## Guardrails checklist (run through this before writing prose)

- [ ] Am I in the supervised `/mcs` path (not cron/hook)?
- [ ] Did the human ask for or approve this specific prose change?
- [ ] Does the change avoid deleting any hand-written note?
- [ ] Does the change avoid making prose more verbose?
- [ ] Is the diff reviewable (small, scoped, explained)?

If any box is unchecked, queue a proposal in `log.md` instead of writing.
