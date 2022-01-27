create table nomz.recipe_cache (
    link text not null primary key,
    data bytea not null,
    updated timestamptz not null
);
