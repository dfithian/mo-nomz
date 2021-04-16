truncate nomz.user cascade;

alter table nomz.user
  drop column username,
  add column token text not null,
  add column created timestamptz not null default now(),
  add column is_valid boolean not null;

drop index if exists nomz.user__username;
