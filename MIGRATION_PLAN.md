# Persistence — where data lives

## Current design

Storage is chosen at boot by the shim in `index.html`, from two backends behind
one interface (`entries(prefix)` / `set(key, value)` / `del(key)`):

| Backend    | When                                                | Scope             |
| ---------- | --------------------------------------------------- | ----------------- |
| `supabase` | signed in, and `supabase-config.js` has real values | the account       |
| `local`    | signed out, unconfigured, or Supabase unreachable   | that browser only |

Signing in gates **sync, not the app**. A visitor arriving from a link gets a
working tracker on `localStorage`; signing in moves the same data to an account
that follows you across devices.

The Elm side is unaware of all this — it still speaks the five ports in
`Storage.elm`, keyed by `progressive_ex_{ABBR}`, `progressive_routines_v1`,
and `progressive_draft_v1`.

## Schema

One table, `public.kv`, mirroring those keys — see
`supabase/migrations/20260829000000_kv_store.sql`.

Keeping the key/value shape is what confines the backend swap to the shim.
`value` is `jsonb` rather than `text`, so history is still queryable in SQL:

```sql
select key, jsonb_array_length(value -> 'sessions') as sessions
from kv where key like 'progressive_ex_%';
```

Two properties matter:

- **Primary key `(user_id, key)`** — two accounts holding the same key never
  collide, so one project is safe for more than one person.
- **RLS granted `to authenticated` only** — the anon role has no access at all.
  A signed-out visitor cannot read or write the table even though the anon key
  is public. That key is _meant_ to ship in client code; the policy is what
  protects the data, which is why `using (true)` would be a mistake here.

## Setting up a hosted project

1. Create a project at https://supabase.com.
2. **Settings → API**: copy the Project URL and the `anon public` key into
   `supabase-config.js`. Never put the `service_role` key there.
3. Push the schema — `npx supabase link --project-ref <ref>` then
   `npx supabase db push`, or paste the migration into the SQL Editor.
4. **Authentication → URL Configuration**: add the deployed URL to the redirect
   allow-list. Magic links silently fall back to `site_url` otherwise. The
   local equivalents are in `supabase/config.toml`.

Left as placeholders, `supabase-config.js` keeps the app on `localStorage` and
hides the sync control, so the repo still runs standalone.

## Moving existing data in

Signing in on a device that has local history uploads it automatically, guarded
on the account being completely empty — so signing in somewhere with stale data
can never clobber good data. See `seedFromLocalIfEmpty` in `index.html`.

That covers `localStorage`. It does **not** cover data held in the Claude
Artifact's `window.storage`, which no longer has a code path here. Getting that
out needs the Export button in Manage, which is currently a stub
(`CopyExport -> noOp` in `src/Page/Manage.elm`) — it renders and does nothing.

## Local development

```bash
npx supabase start          # Postgres + PostgREST + Auth on :54321
npx supabase db reset       # reapply migrations
npx supabase stop
```

`npx supabase status` prints the local URL and anon key for `supabase-config.js`.

## Working on the shim

Two things in `index.html` are load-bearing and easy to reintroduce as bugs:

- **`storageLoaded` must fire exactly once.** `Main.update` rebuilds the Log and
  History sub-models from it, so a second fire mid-session wipes the screen.
  The backend is therefore chosen _before_ the first load, and a sign-in
  reloads the page rather than swapping backends under a live session.
- **Never await the Supabase client inside `onAuthStateChange`.** supabase-js
  invokes that callback while holding its auth lock; touching the client from
  inside deadlocks `getSession()` and hangs the boot on "Loading…". Defer with
  `setTimeout(..., 0)` first.

## Future options

The backend interface is the seam — any of these is a drop-in replacement:
self-hosted PostgREST, Cloudflare Workers + D1, or local-first PGlite with sync.
