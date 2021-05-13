# Mo Nomz

A Haskell / Swift app which will scrape provided recipe URLs ingredients and aggregate them into grocery lists.

## Frontend

```bash
cd client/web
npx gatsby build
cp -R public/* ../../server/mo-nomz/assets
```

## Start the server

```bash
stack run mo-nomz
```

## Set up database

```bash
initdb -D tmp
pg_ctl -D tmp -l postgres.log start
psql -d postgres -c "create user postgres"
psql -d postgres -c "alter user postgres superuser"
psql -d postgres -c "grant create on database postgres to postgres"
psql -d postgres -U postgres -f sql/init.sql
```

## Use the Scraper

```haskell
import Control.Monad (fail)
import Control.Monad.Except (runExceptT)
import Network.URI (parseURI)

let x = "https://www.allrecipes.com/recipe/26317/chicken-pot-pie-ix/"
let y = "https://www.pillsbury.com/recipes/classic-chicken-pot-pie/1401d418-ac0b-4b50-ad09-c6f1243fb992"
let z = "https://www.tasteofhome.com/recipes/favorite-chicken-potpie/"
let w = "https://www.foodnetwork.com/recipes/ina-garten/perfect-roast-chicken-recipe-1940592"
let v = "https://rachlmansfield.com/paleo-chocolate-chip-banana-bread/"
let u = "https://sallysbakingaddiction.com/chocolate-lava-cakes/"
let t = "https://cafedelites.com/chicken-tikka-masala/"
let s = "https://cooking.nytimes.com/recipes/1017256-french-onion-soup"
runExceptT $ scrapeUrl =<< maybe (fail "missing") pure (parseURI x)
```

## Notes

Non food related tags:
- https://greenkitchenstories.com/
- http://www.mymoroccanfood.com/
- http://annajones.co.uk/
- https://eatlikeagirl.com/
- https://the-fit-foodie.com/
- https://www.allasyummyfood.com/
- https://www.mynewroots.org/site/
- https://givemesomespice.com/
- http://eatmeerecipes.co.za/
- http://mynameisyeh.com/
- https://greenkitchenstories.com/recipe-index/
- http://www.sproutedkitchen.com/

## Test plan

* Profile
    * Contact support
    * Support form
    * Email directly
* Grocery list
    * Export
        * To buy only
    * Add
        * Link
        * Structured
        * Pasted
    * Move to bought
    * Move to to buy
    * Edit
    * Merge
    * Delete
    * Clear
    * Reorder
* Recipe list
    * Move to saved
    * Move to active
    * Delete
