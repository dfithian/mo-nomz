drop schema if exists nomz cascade;

create schema nomz;

create extension if not exists citext;

create table nomz.user (
  id bigserial primary key,
  username citext not null
);

create unique index user__username on nomz.user(username);

create table nomz.recipe (
  id bigserial primary key,
  user_id bigint not null references nomz.user(id),
  name varchar(50) not null,
  link text not null,
  active boolean not null
);

create table nomz.recipe_ingredient (
  id bigserial primary key,
  recipe_id bigserial not null references nomz.recipe(id),
  name citext not null,
  quantity real not null,
  unit citext not null
);

create index recipe_ingredient__recipe_id on nomz.recipe_ingredient(recipe_id);

create table nomz.ingredient (
  id bigserial primary key,
  user_id bigint not null references nomz.user(id),
  name citext not null,
  quantity real not null,
  unit citext not null
);
