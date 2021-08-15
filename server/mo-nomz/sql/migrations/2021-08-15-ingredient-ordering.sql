alter table nomz.ingredient
  add column ordering int null;
update nomz.ingredient
   set ordering = default_ordering.ordering
  from (
    select
      id as id,
      row_number() over (partition by user_id, recipe_id) as ordering
    from nomz.ingredient
    where recipe_id is not null
  ) default_ordering
 where nomz.ingredient.id = default_ordering.id;
