# progressive/

Workout tracker — Elm frontend, Supabase or `localStorage` persistence.

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

## Running it

```bash
npm run build
npx http-server -p 8080 .    # index.html + dist/index.js must be served together
```

Storage is chosen at boot: an account via Supabase when signed in, otherwise
`localStorage` for that browser. With `supabase-config.js` left at its
placeholder values the app runs standalone on `localStorage` and hides the sync
control. See `MIGRATION_PLAN.md`.

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
index.html              Shell with Tailwind CDN + storage shim + sync UI
supabase-config.js      Project URL + anon key (both public; placeholders by default)
supabase/               Local stack config and SQL migrations
elm.json                Elm project config
package.json            npm build script
```

## Storage keys

| Key                       | Contents                             |
| ------------------------- | ------------------------------------ |
| `progressive_ex_{ABBR}`   | Exercise data (one key per exercise) |
| `progressive_routines_v1` | All routines                         |
| `progressive_draft_v1`    | In-progress session draft            |

Keys are discovered by prefix rather than hardcoded, so an exercise added in
Manage is found again on reload.

## Supabase

Schema and setup steps live in `MIGRATION_PLAN.md`; the migration is in
`supabase/migrations/`. `npx supabase start` runs the whole stack locally.
