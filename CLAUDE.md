# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```bash
# Build
npm run build                          # elm make src/Main.elm --output=dist/index.js

# Type-check without producing output
elm make src/Main.elm --output=/dev/null

# Format (auto-fix)
npx elm-format src/ --yes
npx prettier --write index.html

# Format (CI check — fails if anything is unformatted)
npx elm-format --validate src/
npx prettier --check index.html
```

There are no tests. CI runs format checks + build on push/PR to main.

## Architecture

This is a single-page Elm 0.19.1 app compiled to `dist/index.js` and loaded by `index.html`.

**Data flow:**

```
index.html (JS shim)
    │  flags: { today: "YYYY-MM-DD" }
    ▼
Main.elm  ←──── Storage ports (storageLoaded)
    │
    ├── Page.Log      — session logging flow (pick routine → log sets → confirm)
    ├── Page.History  — calendar / by-exercise / by-routine views
    └── Page.Manage   — routine and exercise CRUD
```

**State split:** `Main.Model` holds `Db`, `Routines`, `today`, and one sub-model per page. Pages are purely functional — they receive `Db` and `Routines` as arguments and return effects rather than issuing `Cmd`s directly. `Main.update` interprets page effects and issues the appropriate storage port commands.

**Page effect pattern:** Each page exposes an `update*` function returning `( PageModel, Maybe PageEffect )` (Log/History) or a `ManageUpdate` record (Manage, which can also mutate `Db`/`Routines`). `Main` unwraps effects and calls the relevant `Storage` port.

**Storage:** All persistence goes through Elm ports defined in `Storage.elm`. The JS shim in `index.html` bridges to `window.storage` (Claude Artifact) with a `localStorage` fallback. Data is stored per-exercise (`progressive_ex_{ABBR}`), plus one key for routines and one for the in-progress session draft.

**Msg types** (`LogPageMsg`, `HistoryPageMsg`, `ManagePageMsg`) are defined in `Model.elm` and wrapped by `Main`'s `Msg` variants (`LogMsg`, `HistoryMsg`, `ManageMsg`). Pages import `Model exposing (..)` to use them directly.

## Key conventions

- `elm-stuff/` is Elm's build cache — treat it like `node_modules/`, never read or modify it.
- All Elm source lives under `src/`. The `elm.json` `source-directories` points to `src/`.
- Styling is Tailwind CDN (no build step) with DM Sans / DM Mono from Google Fonts.
- Exercise keys are short abbreviations (e.g. `"LP"`, `"BP"`); routine keys are arbitrary strings. Both are `Dict String _` in `Model`.
