module ServerSpec where

import NomzPrelude

import Chez.Grater.Test.ParsedIngredients (allRecipesIngredients, allRecipesSteps)
import Chez.Grater.Types
  ( Ingredient(..), IngredientName(..), Quantity(..), RecipeName(..), Step(..), Unit(..)
  )
import Data.Serialize (encode)
import Database.PostgreSQL.Simple (Binary(Binary), execute)
import Test.Hspec (Spec, describe, it, shouldBe)

import Foundation (App(..), cacheSettings)
import Settings (AppSettings(..), CacheSettings(..))
import TestEnv (Env(..), runEnv, runServer)
import Types (RecipeLink(..), ScrapedRecipe(..))
import qualified Database

import Server

cacheSpec :: Env -> Spec
cacheSpec env = describe "Cache" $ do
  let emptyCache = CacheSettings True 0 0
      link = RecipeLink "https://www.allrecipes.com/recipe/26317/chicken-pot-pie-ix/"
      allrecipes = ScrapedRecipe
        { scrapedRecipeName = RecipeName "Chicken Pot Pie IX Recipe | Allrecipes"
        , scrapedRecipeIngredients = allRecipesIngredients
        , scrapedRecipeSteps = allRecipesSteps
        }
      fake = ScrapedRecipe
        { scrapedRecipeName = RecipeName "fake"
        , scrapedRecipeIngredients = [Ingredient (IngredientName "fake") (Quantity 1.0) (Unit "cup")]
        , scrapedRecipeSteps = [Step "fake"]
        }

  it "fetches from the cache" $ do
    runEnv env $ \c -> do
      now <- getCurrentTime
      void $ execute c "insert into nomz.recipe_cache (link, data, updated) values (?, ?, ?)" (link, Binary (encode fake), now)
    actual <- runServer env $ scrapeUrlCached link
    actual `shouldBe` fake

  it "creates if it doesn't exist" $ do
    runServer env $ void $ scrapeUrlCached link
    actual <- runEnv env $ \c -> Database.selectCachedRecipe c link
    actual `shouldBe` Just allrecipes

  it "creates if it is expired" $ do
    CacheSettings {..} <- runServer env $ asks cacheSettings
    expired <- addUTCTime (negate (fromIntegral (cacheSettingsValidSeconds + 1))) <$> getCurrentTime
    runEnv env $ \c -> void $ execute c "insert into nomz.recipe_cache (link, data, updated) values (?, ?, ?)" (link, Binary (encode fake), expired)
    actual <- runServer env $ local (\app -> app { appSettings = (appSettings app) { appCache = emptyCache }}) $ scrapeUrlCached link
    actual `shouldBe` allrecipes
