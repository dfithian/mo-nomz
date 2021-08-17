alter table nomz.user
  add column last_active timestamptz not null default now();
