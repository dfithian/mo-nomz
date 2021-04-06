drop schema if exists nomz cascade;

create schema nomz;

create table nomz.recipe (
  id bigserial primary key,
  name varchar not null,
  link varchar null
);

create index recipe__name on nomz.recipe(name);

create table nomz.ingredient (
  id bigserial primary key,
  recipe_id bigint not null references nomz.recipe(id),
  name varchar(50) not null,
  quantity real not null,
  unit varchar(20) not null
);

create index ingredient__recipe_id on nomz.ingredient(recipe_id);
