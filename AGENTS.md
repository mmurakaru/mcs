# AGENTS.md - the mcs operating contract

This file is the canonical, vendor-neutral contract for any agent working in an
mcs vault. `CLAUDE.md`, `.cursorrules`, and friends are thin pointers to it.

mcs (Man-Computer Symbiosis) is a personal wiki framework. The human jots notes
freely, with zero ceremony. The agent is a **bookkeeper**: it periodically
reconciles whatever was added - typing it, backfilling frontmatter, cross-linking,
indexing, and flagging contradictions - **without ever deleting a note or making
it more verbose.**

## The one rule that overrides everything

> **Never delete a hand-written note. Never make hand-written prose more verbose.**

If a reconcile would remove or pad human prose, stop and queue a proposal instead.
Frontmatter is yours to maintain; the body below the frontmatter is the human's.

## Capture is frictionless

The human writes notes with **no frontmatter** (or just a title). That is not an
error - it means "not yet reconciled". Do not prompt the human to add metadata at
capture time. You backfill it later.

## One engine, three triggers (tiered by blast radius)

A single idempotent `reconcile` engine, invoked three ways, each allowed to touch
progressively more:

| Trigger | Allowed to | Mechanism |
|---|---|---|
| **pre-commit hook** | nothing (read-only) | `mcs validate` - fails the commit on a malformed (post-reconcile) note |
| **cron / scheduled** | additive only | `mcs reconcile` (safe) - backfill frontmatter, rebuild index, queue proposals to `log.md`. **Never edits prose.** |
| **skill `/mcs`** | rewrite/merge prose | supervised, with review - the **only** path allowed to touch the body |

When in doubt about which tier you are in: if you were not invoked by a human
through `/mcs`, you may not edit prose.

## The type taxonomy (a stack of proven standards)

`type` = *what a note is*. The folder = *where it lives* (PARA). They are
orthogonal. Every `type` traces to a published source:

- **Diataxis** - `tutorial`, `how-to`, `reference`, `explanation` (doc genres)
- **MADR** - `decision` (status-driven; an FDR and an RFC are both a `decision`, distinguished by `status`)
- **Zettelkasten / Evergreen** - `note` (atomic, densely-linked capture primitive; the default)
- **llm-wiki** - `entity`, `concept`, `index`, `log` (the agent-maintained synthesis layer)

Folders follow **PARA**: `projects/`, `areas/`, `resources/`, `archive/`.

Orthogonal fields:
- `owner`: `human` | `agent` | `shared`
- `status`: `draft` | `current` | `proposed` | `accepted` | `superseded` | `deprecated`

## Frontmatter contract (lean-required; you backfill)

Required after a reconcile pass (this is exactly what `mcs validate` enforces):

```yaml
id:       projects-note-some-slug   # <area>-<type>-<slug>, stable, never renumbered
type:     note                      # from the taxonomy above
owner:    human                     # human | agent | shared
status:   draft                     # lifecycle state
summary:  One-line synopsis.        # backfilled from the first prose line if absent
updated:  2026-06-28                # date of last reconcile (YYYY-MM-DD)
```

Optional / rich (add when useful, never required): `tags`, `related` (by id),
`sources`, `last_verified`.

The language-neutral version of these rules lives in
[`schema/frontmatter.schema.json`](schema/frontmatter.schema.json) so non-OCaml
tools can read them.

### id rules

- Format: `<area>-<type>-<slug>`. The area is the top-level PARA folder.
- **Stable. Never renumbered** (MADR convention). If a note moves folders, the id
  does not change; the index maps id -> current path.
- Lowercase ASCII, hyphen-separated, no leading/trailing/double hyphen.

## Links and portability

- Body links use the vault's configured `linkStyle` (this vault: **hybrid** -
  `[[wikilinks]]` in prose for Obsidian's graph, plus the portable layer:
  `related:` by stable id and a generated `index.json` mapping id -> path).
- Surface syntax is swappable losslessly: `mcs reconcile --convert-links <style>`
  resolves every link through `index.json` and rewrites it. Stable ids are what
  make this safe.

## The reconcile pass, concretely

1. **Backfill** missing required frontmatter, additively (never overwrite a value
   the human set). Default `type` is `note`; defaults for `owner`/`status` follow
   the type.
2. **Rebuild** `index.json` (machine) and `_index.md` (human MOC).
3. **Queue proposals** - orphans, likely contradictions, stale claims, missing
   concept pages - as append-only entries in a `log` note. Never act on them in
   the safe tier; that is the human-supervised `/mcs` tier's job.

The pass is idempotent: running it twice over an unchanged vault changes nothing.

## CLI

```
mcs validate [path]                       # schema-check required frontmatter (no writes; the hook)
mcs index [path]                          # rebuild index.json + _index.md
mcs scaffold <type> "<title>" [--area A]  # create a new typed note
mcs reconcile [path] [--convert-links S]  # additive backfill + index rebuild
```

## References

The taxonomy is assembled, not invented. Each layer traces to:

- llm-wiki (Karpathy) - https://gist.github.com/karpathy/442a6bf555914893e9891c11519de94f
- Diataxis (Procida) - https://diataxis.fr/
- MADR - https://adr.github.io/madr/
- Evergreen notes (Matuschak) - https://notes.andymatuschak.org/Evergreen_notes
- PARA (Forte) - https://fortelabs.com/blog/para/
- Man-Computer Symbiosis (Licklider, 1960) - https://groups.csail.mit.edu/medg/people/psz/Licklider.html
- Memex (Bush, 1945) - https://en.wikipedia.org/wiki/Memex
