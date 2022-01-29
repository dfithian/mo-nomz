create table nomz.recipe_cache (
    link text not null primary key,
    data bytea not null,
    updated timestamptz not null,
    ingredient_scrape_name text null,
    ingredient_scrape_version int null,
    step_scrape_name text null,
    step_scrape_version int null
);
