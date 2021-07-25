alter table nomz.recipe
  add column rating int not null default 0;
alter table nomz.recipe
  alter column rating drop default;
alter table nomz.recipe
  add column notes text not null default '';
alter table nomz.recipe
  alter column notes drop default;
