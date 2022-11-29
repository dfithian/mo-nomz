module DatabaseSpec where

import NomzPrelude

import Chez.Grater.Scraper.Types (ScrapeMeta(..), ScrapeMetaWrapper(..), ScrapeName(..), inception)
import Chez.Grater.Test.Gen (arbitraryIngredient, arbitraryRecipeName, arbitraryStep)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (generate, listOf1)

import Auth (BcryptedAuthorization(..))
import TestEnv (Env(..), runEnv)
import Types (RecipeLink(..), ScrapedRecipe(..))

import Database

spec :: Env -> Spec
spec env = describe "Database" $ do
  it "health" $ runEnv env health

  it "insertToken then fetchToken" $ do
    let token = BcryptedAuthorization "foobar"
    actual <- runEnv env $ \c -> do
      userId <- insertToken c token
      fetchToken c userId
    actual `shouldBe` Just token

  it "selectCachedRecipe" $ do
    name <- generate $ arbitraryRecipeName
    ingredients <- generate $ listOf1 arbitraryIngredient
    steps <- generate $ listOf1 arbitraryStep
    let link = RecipeLink "foo"
        ingredientMeta = ScrapeMeta (ScrapeName "fooI") inception
        stepMeta = ScrapeMeta (ScrapeName "fooS") inception
        recipe = ScrapedRecipe name ingredients steps
        info = ScrapeMetaWrapperIngredientAndStep ingredientMeta stepMeta
    actual <- runEnv env $ \c -> do
      repsertCachedRecipe c link recipe info
      selectCachedRecipe c link
    actual `shouldBe` Just recipe

  it "repsertCachedRecipe" $ do
    name <- generate $ arbitraryRecipeName
    ingredients <- generate $ listOf1 arbitraryIngredient
    steps <- generate $ listOf1 arbitraryStep
    let link1 = RecipeLink "foo"
        link2 = RecipeLink "bar"
        ingredientMeta = ScrapeMeta (ScrapeName "fooI") inception
        stepMeta = ScrapeMeta (ScrapeName "fooS") inception
        recipe = ScrapedRecipe name ingredients steps
        info1 = ScrapeMetaWrapperIngredient ingredientMeta
        info2 = ScrapeMetaWrapperIngredientAndStep ingredientMeta stepMeta
    runEnv env $ \c -> do
      repsertCachedRecipe c link1 recipe info1
      repsertCachedRecipe c link2 recipe info2

  it "refreshCachedRecipes" $ do
    name <- generate $ arbitraryRecipeName
    ingredients <- generate $ listOf1 arbitraryIngredient
    steps <- generate $ listOf1 arbitraryStep
    let link1 = RecipeLink "foo"
        link2 = RecipeLink "bar"
        ingredientMeta = ScrapeMeta (ScrapeName "fooI") inception
        stepMeta = ScrapeMeta (ScrapeName "fooS") inception
        recipe = ScrapedRecipe name ingredients steps
        info = ScrapeMetaWrapperIngredientAndStep ingredientMeta stepMeta
    (actualTooBig, actualValid) <- runEnv env $ \c -> do
      repsertCachedRecipe c link1 recipe info
      repsertCachedRecipe c link2 recipe info
      refreshCachedRecipes c 1
      tooBig <- selectCachedRecipe c link2
      valid <- selectCachedRecipe c link1
      pure (tooBig, valid)
    actualTooBig `shouldBe` Nothing
    actualValid `shouldBe` Just recipe

  it "invalidateCachedRecipes" $ do
    name <- generate $ arbitraryRecipeName
    ingredients <- generate $ listOf1 arbitraryIngredient
    steps <- generate $ listOf1 arbitraryStep
    let link1 = RecipeLink "foo"
        link2 = RecipeLink "bar"
        info1 = ScrapeMetaWrapperIngredient (ScrapeMeta (ScrapeName "foo") inception)
        info2 = ScrapeMetaWrapperIngredient (ScrapeMeta (ScrapeName "bar") inception)
        recipe = ScrapedRecipe name ingredients steps
    (actualInvalid, actualValid) <- runEnv env $ \c -> do
      repsertCachedRecipe c link1 recipe info1
      repsertCachedRecipe c link2 recipe info2
      invalidateCachedRecipes c ((==) info1)
      (,)
        <$> selectCachedRecipe c link1
        <*> selectCachedRecipe c link2
    actualInvalid `shouldBe` Nothing
    actualValid `shouldBe` Just recipe
