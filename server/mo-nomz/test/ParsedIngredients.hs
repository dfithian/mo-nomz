module ParsedIngredients where

import ClassyPrelude
import qualified Data.CaseInsensitive as CI

import Types (Ingredient(..), IngredientName(..), Quantity(..), Unit(..))

allParsedIngredients :: [[Ingredient]]
allParsedIngredients =
  [ allRecipesIngredients
  , pillsburyIngredients
  , tasteOfHomeIngredients
  , rachelMansfieldIngredients
  , foodNetworkIngredients
  ]

pureIngredient :: Double -> Text -> Text -> Ingredient
pureIngredient q u i = Ingredient
  { ingredientName = IngredientName $ CI.mk i
  , ingredientQuantity = Quantity q
  , ingredientUnit = Unit $ CI.mk u
  , ingredientActive = True
  }

pureIngredientNoQuantity :: Text -> Text -> Ingredient
pureIngredientNoQuantity u i = Ingredient
  { ingredientName = IngredientName $ CI.mk i
  , ingredientQuantity = QuantityMissing
  , ingredientUnit = Unit $ CI.mk u
  , ingredientActive = True
  }

pureIngredientNoUnit :: Double -> Text -> Ingredient
pureIngredientNoUnit q i = Ingredient
  { ingredientName = IngredientName $ CI.mk i
  , ingredientQuantity = Quantity q
  , ingredientUnit = UnitMissing
  , ingredientActive = True
  }

pureIngredientName :: Text -> Ingredient
pureIngredientName i = Ingredient
  { ingredientName = IngredientName $ CI.mk i
  , ingredientQuantity = QuantityMissing
  , ingredientUnit = UnitMissing
  , ingredientActive = True
  }

allRecipesIngredients :: [Ingredient]
allRecipesIngredients =
  [ pureIngredient 1 "pound" "skinless, boneless chicken breast halves - cubed"
  , pureIngredient 1 "cup" "sliced carrots"
  , pureIngredient 1 "cup" "frozen green peas"
  , pureIngredient 0.5 "cup" "sliced celery"
  , pureIngredient (1 / 3) "cup" "butter"
  , pureIngredient (1 / 3) "cup" "chopped onion"
  , pureIngredient (1 / 3) "cup" "all-purpose flour"
  , pureIngredient 0.5 "tsp" "salt"
  , pureIngredient 0.25 "tsp" "black pepper"
  , pureIngredient 0.25 "tsp" "celery seed"
  , pureIngredient 1.75 "cup" "chicken broth"
  , pureIngredient (2 / 3) "cup" "milk"
  , pureIngredientNoUnit 2 "(9 inch)  unbaked pie crusts"
  ]

pillsburyIngredients :: [Ingredient]
pillsburyIngredients =
  [ pureIngredient 1 "box" "pillsbury\\8482 refrigerated pie crusts, softened as directed on box"
  , pureIngredient (1 / 3) "cup" "butter or margarine"
  , pureIngredientNoQuantity "cup" "chopped onion"
  , pureIngredientNoQuantity "cup" "all-purpose flour"
  , pureIngredient 0.5 "tsp" "salt"
  , pureIngredient 0.25 "tsp" "pepper"
  , pureIngredient 1.75 "cup" "progresso\\8482 chicken broth (from 32-oz carton)"
  , pureIngredientNoQuantity "cup" "milk"
  , pureIngredient 2.5 "cup" "shredded cooked chicken or turkey"
  , pureIngredient 2 "cup" "frozen mixed vegetables, thawed"
  ]

tasteOfHomeIngredients :: [Ingredient]
tasteOfHomeIngredients =
  [ pureIngredient 2 "cup" "diced peeled potatoes"
  , pureIngredient 1.75 "cup" "sliced carrots"
  , pureIngredient 1 "cup" "butter, cubed"
  , pureIngredient (2 / 3) "cup" "chopped onion"
  , pureIngredient 1 "cup" "all-purpose flour"
  , pureIngredient 1.75 "tsp" "salt"
  , pureIngredient 1 "tsp" "dried thyme"
  , pureIngredient 0.75 "tsp" "pepper"
  , pureIngredient 3 "cup" "chicken broth"
  , pureIngredient 1.5 "cup" "whole milk"
  , pureIngredient 4 "cup" "cubed cooked chicken"
  , pureIngredient 1 "cup" "frozen peas"
  , pureIngredient 1 "cup" "frozen corn"
  , pureIngredientNoUnit 4 "sheets refrigerated pie crust"
  ]

rachelMansfieldIngredients :: [Ingredient]
rachelMansfieldIngredients =
  [ pureIngredient (1 / 3) "cup" "+ 2 tablespoons coconut flour"
  , pureIngredient 2 "tbsp" "coconut flour"
  , pureIngredientNoUnit 3 "eggs at room temperature"
  , pureIngredient 1 "tbsp" "maple syrup"
  , pureIngredientNoUnit 3 "medium/large ripe bananas mashed"
  , pureIngredient 1 "tbsp" "melted & cooled coconut oil"
  , pureIngredient 0.5 "tsp" "of baking powder"
  , pureIngredient 0.5 "cup" "of dark chocolate chips"
  , pureIngredient 0.5 "cup" "sunbutter (or your nut butter of choice)"
  , pureIngredientNoQuantity "sprinkle" "of cinnamon"
  , pureIngredientNoQuantity "splash" "of vanilla extract"

  -- FIXME
  , pureIngredient (1 / 3) "cup" "+"
  , pureIngredientName "dry ingredients:"
  , pureIngredientName "topping:"
  , pureIngredientName "wet ingredients:"
  ]

foodNetworkIngredients :: [Ingredient]
foodNetworkIngredients =
  [ pureIngredientNoUnit 1 "(5 to 6 pound) roasting chicken"
  , pureIngredientName "kosher salt"
  , pureIngredientName "freshly ground black pepper"
  , pureIngredientNoUnit 1 "large bunch fresh thyme, plus 20 sprigs"
  , pureIngredientNoUnit 1 "lemon, halved"
  , pureIngredientNoUnit 1 "head garlic, cut in half crosswise"
  , pureIngredient 2 "tbsp" "(1/4 stick) butter, melted"
  , pureIngredientNoUnit 1 "large yellow onion, thickly sliced"
  , pureIngredientNoUnit 4 "carrots cut into 2-inch chunks"
  , pureIngredientNoUnit 1 "bulb of fennel, tops removed, and cut into wedges"
  , pureIngredientName "olive oil"
  ]
