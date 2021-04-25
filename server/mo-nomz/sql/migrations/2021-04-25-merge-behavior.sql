alter table nomz.ingredient drop constraint ingredient_grocery_id_fkey;
alter table nomz.ingredient add constraint ingredient_grocery_id_fkey foreign key (grocery_id) references nomz.grocery_item(id);
