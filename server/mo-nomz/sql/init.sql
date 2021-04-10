drop schema if exists nomz cascade;

create schema nomz;

create extension if not exists citext;

create table nomz.user (
  id bigserial primary key,
  username citext not null
);

create unique index user__username on nomz.user(username);

create table nomz.ingredient (
  id bigserial primary key,
  user_id bigint not null references nomz.user(id),
  name citext not null,
  quantity real not null,
  unit citext not null
);
