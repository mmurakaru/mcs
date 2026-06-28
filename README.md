# mcs - Man-Computer Symbiosis

> A tool-agnostic personal wiki co-authored by human and agent - typed,
> cross-linked, and kept current.

mcs turns a pile of markdown notes into a scalable, navigable wiki for both humans
and agents. **You write; the machine keeps the books.** You jot notes freely, with
no ceremony and no prompting. An agent acts as a *bookkeeper* that periodically
reconciles whatever you added - typing it, backfilling frontmatter, cross-linking,
indexing, and flagging contradictions - **without ever deleting your notes or
making them more verbose.**

Hand-written notes and agent-generated synthesis live in one framework; the agent
reads and writes both.

## The name

It is an homage to J.C.R. Licklider's 1960 paper
[*Man-Computer Symbiosis*](https://groups.csail.mit.edu/medg/people/psz/Licklider.html),
which imagined a close-coupled partnership where the human sets the goals and
frames the questions while the machine does the routine work that makes the
thinking possible. A bookkeeping agent over your notes is that thesis, literally.
The lineage runs back further, to Vannevar Bush's 1945
[Memex](https://en.wikipedia.org/wiki/Memex) - a personal, associative store of
everything you know.

## How it works

You capture notes with **zero friction** - no required frontmatter, no agent
prompt. A note with no frontmatter just means "not yet reconciled". Later, a
reconcile pass backfills the metadata and links.

### One engine, three triggers

A single idempotent `reconcile` engine, invoked three ways, each tiered by how
much it is allowed to touch:

- **pre-commit hook** -> `mcs validate` only. No writes. Fails a commit on a
  malformed note.
- **cron / scheduled** -> additive `reconcile`. Backfills frontmatter, rebuilds
  the index, queues proposals for review. **Never edits prose.**
- **skill `/mcs`** -> supervised `reconcile`. The only path allowed to rewrite or
  merge hand-written prose, always with review.

The one rule across all of them: **never delete a hand-written note; never make it
more verbose.**

### Typed on proven standards (nothing invented)

`type` is *what a note is*; the folder is *where it lives* (orthogonal). Every type
traces to a published source:

| Layer | Source | Types |
|---|---|---|
| Doc genres | [Diataxis](https://diataxis.fr/) | `tutorial` `how-to` `reference` `explanation` |
| Decisions | [MADR](https://adr.github.io/madr/) | `decision` |
| Atomic capture | [Evergreen notes](https://notes.andymatuschak.org/Evergreen_notes) | `note` |
| Agent synthesis | [llm-wiki](https://gist.github.com/karpathy/442a6bf555914893e9891c11519de94f) | `entity` `concept` `index` `log` |
| Folders | [PARA](https://fortelabs.com/blog/para/) | `projects/` `areas/` `resources/` `archive/` |

### Tool-agnostic and portable

Obsidian today, a bare IDE tomorrow. Body links use a configurable `linkStyle`
(`wikilink` | `markdown` | `hybrid`); stable ids plus a generated `index.json`
mean you can convert link styles losslessly with
`mcs reconcile --convert-links <style>` - no broken links, no manual rework.

## The engine

mcs is written in **OCaml**: the taxonomy is an exhaustive variant type, so
`validate`/`reconcile` are compiler-checked pattern matches. Adding a type forces
you to handle it everywhere - the build fails until you do (correct-by-construction).
The engine has **no runtime dependencies** beyond the OCaml standard library.

```
mcs validate [path]                       # schema-check required frontmatter (no writes)
mcs index [path]                          # rebuild index.json + _index.md
mcs scaffold <type> "<title>" [--area A]  # create a new typed note
mcs reconcile [path] [--convert-links S]  # additive backfill + index rebuild
```

### Install

Adopters download a prebuilt single binary from
[Releases](../../releases) (macOS arm64/x64, Linux) - no OCaml toolchain needed.

Contributors build from source:

```sh
dune build      # build the mcs binary -> _build/default/bin/mcs.exe
dune test       # run the test suite
```

## Adopt it

mcs ships as a standalone GitHub **template repo**:

```sh
gh repo create my-vault --template <owner>/mcs --public --clone
```

The contract for any agent working in a vault is **[AGENTS.md](AGENTS.md)**.

## Layout

```
README.md                        # this file
AGENTS.md                        # canonical agent operating contract
CLAUDE.md                        # thin pointer -> AGENTS.md
mcs.config.json                  # linkStyle, paths, areas
schema/frontmatter.schema.json   # language-neutral frontmatter contract
doc-templates/                   # one skeleton per type
bin/mcs.ml                       # CLI entry (validate | index | scaffold | reconcile)
lib/                             # taxonomy, frontmatter, validate, index, reconcile, json, ...
test/                            # the test suite
.claude/skills/mcs/              # the /mcs skill
hooks/pre-commit                 # runs `mcs validate`
cron/mcs-safe.example            # scheduled reconcile(safe) example
.github/workflows/             # ci (build + test on PRs) + release (prebuilt binaries)
```

## References

- llm-wiki (Karpathy) - https://gist.github.com/karpathy/442a6bf555914893e9891c11519de94f
- Diataxis (Procida) - https://diataxis.fr/
- MADR - https://adr.github.io/madr/
- Evergreen notes (Matuschak) - https://notes.andymatuschak.org/Evergreen_notes
- PARA (Forte) - https://fortelabs.com/blog/para/
- Man-Computer Symbiosis (Licklider, 1960) - https://groups.csail.mit.edu/medg/people/psz/Licklider.html
- Memex (Bush, 1945, "As We May Think") - https://en.wikipedia.org/wiki/Memex
