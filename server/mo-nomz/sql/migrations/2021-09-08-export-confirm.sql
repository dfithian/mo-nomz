create table nomz.export (
    id bigserial primary key,
    user_id bigint not null references nomz.user(id),
    confirmed_at timestamptz not null
);
