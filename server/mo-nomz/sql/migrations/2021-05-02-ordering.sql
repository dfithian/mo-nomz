alter table nomz.grocery_item
  add column ordering int null;
update nomz.grocery_item
   set ordering = default_ordering.ordering
  from (
    select
      id as id,
      row_number() over (partition by user_id order by name asc) as ordering
    from nomz.grocery_item
  ) default_ordering
 where nomz.grocery_item.id = default_ordering.id;
alter table nomz.grocery_item
  alter column ordering set not null;
