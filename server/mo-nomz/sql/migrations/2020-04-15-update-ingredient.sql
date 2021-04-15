alter table nomz.ingredient add column active boolean null;
update nomz.ingredient set active = true;
alter table nomz.ingredient alter column active set not null;
