create schema nomz;

create extension if not exists citext;

create table nomz.user (
  id bigserial primary key,
  token text not null,
  created timestamptz not null default now(),
  is_valid boolean not null
);

create table nomz.recipe (
  id bigserial primary key,
  user_id bigint not null references nomz.user(id),
  name text not null,
  link text null,
  active boolean not null
);

create table nomz.recipe_ingredient (
  id bigserial primary key,
  recipe_id bigserial not null references nomz.recipe(id),
  name citext not null,
  quantity real null,
  unit citext null
);

create index recipe_ingredient__recipe_id on nomz.recipe_ingredient(recipe_id);

create table nomz.ingredient (
  id bigserial primary key,
  user_id bigint not null references nomz.user(id),
  name citext not null,
  quantity real null,
  unit citext null
);

create table nomz.grocery_item (
  id bigserial primary key,
  user_id bigint not null references nomz.user(id),
  name citext not null,
  quantity real null,
  unit citext null,
  active boolean not null
);
