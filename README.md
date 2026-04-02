# progressive/

Workout tracker — Elm frontend, `window.storage` persistence (Claude Artifact phase).

## Setup

```bash
# Install dependencies
npm install

# Build
npm run build
```

## Development

```bash
# Type-check without building
elm make src/Main.elm --output=/dev/null

# Build with optimizations (for production)
elm make src/Main.elm --output=dist/index.js --optimize
```

## Running in Claude Artifact

1. Build: `npm run build`
2. The output `dist/index.js` + `index.html` need to be served together.
3. In the Claude Artifact context, `window.storage` is available and data persists across sessions.
4. Outside Claude, falls back to `localStorage` automatically.

## File structure

```
src/
  Main.elm              Entry point, top-level model/update/view
  Model.elm             All shared types
  Storage.elm           Port definitions
  Decode.elm            JSON decoders
  Encode.elm            JSON encoders
  Page/
    Log.elm             Routine picker + session logging
    History.elm         Calendar, by-exercise, by-routine
    Manage.elm          Routine + exercise manager
  Component/
    Badge.elm           PB/Stall/Clean/Vol↑ logic
    SetRow.elm          Set input and read-only rows
    Calendar.elm        Calendar grid + stats
dist/
  index.js              Build output
index.html              Shell with Tailwind CDN + port JS shim
elm.json                Elm project config
package.json            npm build script
```

## Storage keys

| Key | Contents |
|-----|----------|
| `progressive_ex_{ABBR}` | Exercise data (one key per exercise) |
| `progressive_routines_v1` | All routines |
| `progressive_draft_v1` | In-progress session draft |

## Migration to Supabase (Phase 2)

Replace the port handlers in `index.html` with `fetch()` calls to the Supabase REST API.
The Elm code is unchanged — only the JS shim changes.
See `MIGRATION_PLAN.md` for the SQL schema and import script.
