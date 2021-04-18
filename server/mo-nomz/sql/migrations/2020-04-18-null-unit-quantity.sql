alter table nomz.ingredient alter column quantity drop not null;
alter table nomz.ingredient alter column unit drop not null;
alter table nomz.recipe_ingredient alter column quantity drop not null;
alter table nomz.recipe_ingredient alter column unit drop not null;
