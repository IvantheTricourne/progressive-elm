# Workout Tracker — Persistence Migration Plan

## Current state (Phase 1)

The app is a Claude Artifact using `window.storage` — Claude's built-in key-value store tied to your account. Works across any device where you're logged into Claude. No external accounts or API keys required.

**Tradeoff:** Data lives inside Claude's infrastructure. Use the Export tab regularly as a backup.

---

## Phase 2 — Supabase migration

### Why migrate?
- Full data ownership, independent of Claude
- Works in any browser, any context (not just Claude app)
- Better for querying large history (years of sessions)
- Free tier is generous (~500MB database, unlimited API calls)

### Step 1 — Create Supabase project (5 min)

1. Go to https://supabase.com and create a free account
2. Create a new project (pick any region close to you)
3. Go to **Settings → API** and copy:
   - `Project URL` (looks like `https://xxxx.supabase.co`)
   - `anon public` key (long JWT string)
4. Go to **SQL Editor** and run this schema:

```sql
create table sessions (
  id uuid primary key default gen_random_uuid(),
  exercise text not null,
  date date not null,
  sets jsonb not null,
  config text,
  created_at timestamptz default now(),
  updated_at timestamptz default now(),
  unique(exercise, date)
);

-- Enable Row Level Security (optional but recommended)
alter table sessions enable row level security;

-- Allow all reads/writes with anon key for now
create policy "allow all" on sessions for all using (true);
```

### Step 2 — Export and import existing data

1. In the app, go to **Export tab** and copy the CSV
2. In Supabase dashboard, go to **Table Editor → sessions → Import**
3. Paste the CSV — map columns: `exercise`, `date`, `weight`, `reps`, `outcome`, `note`
4. Note: the CSV is flat (one row per set); the Supabase schema stores sets as JSONB (one row per session). Use the import script below.

**Import script** (run in Supabase SQL editor after pasting CSV into a temp table):

```sql
-- Assuming you created a temp_import table with the raw CSV columns
insert into sessions (exercise, date, sets)
select
  exercise,
  date::date,
  jsonb_agg(jsonb_build_object(
    'weight', weight::numeric,
    'reps', reps::integer,
    'outcome', outcome,
    'note', note
  ) order by ctid)
from temp_import
group by exercise, date
on conflict (exercise, date) do update
  set sets = excluded.sets, updated_at = now();
```

### Step 3 — Swap the storage module in the app

In the Artifact source, find the `// ─── STORAGE MODULE ───` section and replace the Phase 1 block with:

```js
const SUPABASE_URL = 'https://YOUR_PROJECT.supabase.co';
const SUPABASE_KEY = 'YOUR_ANON_KEY';
const HEADERS = { apikey: SUPABASE_KEY, 'Content-Type': 'application/json' };

function dbToRows(db) {
  const rows = [];
  for (const abbr of Object.keys(db)) {
    for (const sess of db[abbr].sessions) {
      rows.push({ exercise: abbr, date: sess.date, sets: sess.sets, config: db[abbr].defaultConfig });
    }
  }
  return rows;
}

function rowsToDB(rows, seed) {
  const db = JSON.parse(JSON.stringify(seed));
  for (const row of rows) {
    if (!db[row.exercise]) continue;
    db[row.exercise].sessions.push({ date: row.date, sets: row.sets });
    if (row.config) db[row.exercise].defaultConfig = row.config;
  }
  // Sort sessions by date
  for (const abbr of Object.keys(db)) {
    db[abbr].sessions.sort((a, b) => a.date.localeCompare(b.date));
  }
  return db;
}

const storage = {
  async load() {
    const r = await fetch(`${SUPABASE_URL}/rest/v1/sessions?select=*&order=date.asc`, { headers: HEADERS });
    if (!r.ok) return null;
    const rows = await r.json();
    return rows.length ? rowsToDB(rows, SEED) : null;
  },
  async save(db) {
    const rows = dbToRows(db);
    await fetch(`${SUPABASE_URL}/rest/v1/sessions`, {
      method: 'POST',
      headers: { ...HEADERS, Prefer: 'resolution=merge-duplicates' },
      body: JSON.stringify(rows)
    });
  },
  async clear() {
    await fetch(`${SUPABASE_URL}/rest/v1/sessions?exercise=neq.null`, {
      method: 'DELETE', headers: HEADERS
    });
  }
};
```

---

## Storage module interface (contract)

The rest of the app only ever calls these — never call `window.storage` or `fetch` directly from UI code:

```js
await storage.load()   // → db object or null
await storage.save(db) // → void
await storage.clear()  // → void
```

This is what makes swapping backends a one-section change.

---

## Size estimates

| Timeframe | Approx sessions | Data size |
|-----------|----------------|-----------|
| 1 month (current) | ~20 | ~25 KB |
| 1 year | ~240 | ~300 KB |
| 5 years | ~1,200 | ~1.5 MB |

Artifact storage limit: 5MB per key — fine for several years.
Supabase free tier: 500MB — effectively unlimited for this use case.

---

## Future options (Phase 3+)

If you ever want to move again:
- **Google Sheets:** possible with a Cloudflare Worker as an auth proxy
- **Self-hosted Postgres:** swap Supabase URL for your own PostgREST instance
- **Local-first (PGlite):** run Postgres in-browser with sync — emerging option as of 2026

The storage interface contract above means any of these is a drop-in replacement.
