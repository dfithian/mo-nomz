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
let u = "https://sallysbakingaddiction.com/chocolate-lava-cakes/"
let t = "https://cafedelites.com/chicken-tikka-masala/"
let s = "https://cooking.nytimes.com/recipes/1017256-french-onion-soup"
runExceptT $ scrapeUrl =<< maybe (fail "missing") pure (parseURI x)
```

## Notes

- [ ] Better parsing
    - [ ] Add a bunch of links
    - [ ] Paste
    - [ ] Link -> export -> paste?
    - [ ] Platform detection instead of guessing
    - [ ] Smarter scraper - use AST

- https://cooking.nytimes.com/
- https://greenkitchenstories.com/
- http://www.mymoroccanfood.com/
- http://annajones.co.uk/
- https://eatlikeagirl.com/
- https://the-fit-foodie.com/
- https://www.allasyummyfood.com/

- https://www.seriouseats.com
- https://food52.com/
- https://www.thekitchn.com/
- http://www.simplyrecipes.com/
- https://minimalistbaker.com/
- https://www.davidlebovitz.com/
- http://thepioneerwoman.com/
- https://www.skinnytaste.com/
- https://www.twopeasandtheirpod.com/
- https://downshiftology.com/
- http://altonbrown.com/
- http://www.spoonforkbacon.com/
- https://dinnerthendessert.com/
- https://www.acouplecooks.com/
- https://www.loveandoliveoil.com/
- http://www.bakerella.com/
- http://www.howsweeteats.com/
- https://www.browneyedbaker.com/
- https://steamykitchen.com/
- https://www.mybakingaddiction.com/
- https://www.mynewroots.org/site/
- http://ohsheglows.com/
- https://cnz.to/
- https://www.sweetashoney.co/
- https://thestayathomechef.com/blog/
- https://sweetandsavorymeals.com/
- http://joythebaker.com/
- https://givemesomespice.com/
- https://cookilicious.com/
- https://www.chefspencil.com/
- http://eatmeerecipes.co.za/
- http://mynameisyeh.com/
- https://ohmyveggies.com/
- https://ourbestbites.com/
- http://www.thevanillabeanblog.com/
- https://greenkitchenstories.com/recipe-index/
- https://www.slenderkitchen.com/
- https://sugarfreelondoner.com/
- http://foodgawker.com/
- http://sortedfood.com/#!/
- http://www.sproutedkitchen.com/
- http://notwithoutsalt.com/
- http://www.annies-eats.com/
- http://www.bhg.com/recipes/

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
        * Pasted
        * Both
    * Move to bought
    * Move to to buy
    * Edit
    * Merge
    * Delete
* Recipe list
    * Add
        * Link
        * Pasted
        * Both
    * Move to saved
    * Move to active
    * Delete
