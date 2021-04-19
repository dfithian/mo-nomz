# Mo Nomz

A Haskell / Swift app which will scrape provided recipe URLs ingredients and aggregate them into grocery lists.

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
runExceptT $ scrapeUrl =<< maybe (fail "missing") pure (parseURI x)
```
