-- progressive/ — per-user key-value store.
--
-- The Elm app persists through ports that each address a single storage key
-- (progressive_ex_{ABBR}, progressive_routines_v1, progressive_draft_v1).
-- Mirroring that shape here keeps the migration confined to the JS shim: no
-- Elm changes, and the exercise list stops being hardcoded because the keys
-- are now queryable by prefix.

create table if not exists public.kv (
    user_id    uuid        not null default auth.uid() references auth.users (id) on delete cascade,
    key        text        not null check (key <> ''),
    value      jsonb       not null,
    updated_at timestamptz not null default now(),
    primary key (user_id, key)
);

-- The composite primary key is what makes a shared project safe to write to:
-- two users holding the same key never collide, and the upsert in the shim
-- targets (user_id, key).

alter table public.kv enable row level security;

-- `to authenticated` means the anon role has no access at all. Signed-out
-- visitors never reach this table; the shim keeps them on localStorage.
create policy "own rows" on public.kv
    for all
    to authenticated
    using (user_id = auth.uid())
    with check (user_id = auth.uid());

create or replace function public.touch_updated_at()
returns trigger
language plpgsql
set search_path = ''
as $$
begin
    new.updated_at = now();
    return new;
end;
$$;

create trigger kv_touch_updated_at
    before update on public.kv
    for each row execute function public.touch_updated_at();
