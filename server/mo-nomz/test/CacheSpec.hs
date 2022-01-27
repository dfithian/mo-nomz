module CacheSpec where

import ClassyPrelude hiding (link)
import Data.Serialize (encode)
import Data.Time.Clock (addUTCTime)
import Database.PostgreSQL.Simple (Binary(Binary), execute)
import Test.Hspec (Spec, describe, it, shouldBe)

import Scraper.Internal.Types (ScrapedRecipe(..))
import Settings (CacheSettings(..))
import TestEnv (Env(..), runEnv)
import Types
  ( Ingredient(..), IngredientName(..), Quantity(..), RecipeLink(..), RecipeName(..), Step(..)
  , Unit(..)
  )
import qualified Database

import Cache

spec :: Env -> Spec
spec env = describe "Cache" $ do
  let validSeconds = 60 * 60
      settings = CacheSettings
        { cacheSettingsValidSeconds = validSeconds
        }
      link = RecipeLink "foo"
      recipe1 = ScrapedRecipe
        { scrapedRecipeName = RecipeName "foo"
        , scrapedRecipeIngredients = [Ingredient (IngredientName "foo") (Quantity 1.0) (Unit "cup")]
        , scrapedRecipeSteps = [Step "do the thing"]
        }
      recipe2 = ScrapedRecipe
        { scrapedRecipeName = RecipeName "bar"
        , scrapedRecipeIngredients = [Ingredient (IngredientName "bar") (Quantity 1.0) (Unit "cup")]
        , scrapedRecipeSteps = [Step "do the thing"]
        }

  it "fetches from the cache" $ do
    runEnv env $ \c -> do
      now <- getCurrentTime
      void $ execute c "insert into nomz.recipe_cache (link, data, updated) values (?, ?, ?)" (link, Binary (encode recipe1), now)
      recipe <- withCachedRecipe settings c link (pure recipe2) pure
      recipe `shouldBe` recipe1

  it "creates if it doesn't exist" $ do
    runEnv env $ \c -> do
      recipe <- withCachedRecipe settings c link (pure recipe2) pure
      fetched <- Database.selectCachedRecipe c link validSeconds
      recipe `shouldBe` recipe2
      fetched `shouldBe` Just recipe2

  it "creates if it is expired" $ do
    runEnv env $ \c -> do
      expired <- addUTCTime (negate (validSeconds + 1)) <$> getCurrentTime
      void $ execute c "insert into nomz.recipe_cache (link, data, updated) values (?, ?, ?)" (link, Binary (encode recipe1), expired)
      recipe <- withCachedRecipe settings c link (pure recipe2) pure
      recipe `shouldBe` recipe2
