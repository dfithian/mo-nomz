module DatabaseSpec where

import ClassyPrelude hiding (link, link2)

import Test.Hspec (Spec, describe, it, shouldBe, shouldMatchList)
import Test.QuickCheck (generate, listOf1)
import qualified Data.Map as Map

import Auth (BcryptedAuthorization(..))
import Gen
  ( arbitraryGroceryItem, arbitraryIngredient, arbitraryOrderedGroceryItem, arbitraryRecipe
  , arbitraryRecipeName, arbitraryStep
  )
import Scraper.Types (ScrapeInfo(..), ScrapeName(..), ScrapedInfo(..), ScrapedRecipe(..), inception)
import TestEnv (Env(..), runEnv)
import Types (OrderedGroceryItem(..), OrderedIngredient(..), RecipeLink(..), ingredientToGroceryItem)

import Database

spec :: Env -> Spec
spec env@Env {..} = describe "Database" $ do
  it "health" $ runEnv env health

  it "insertToken then fetchToken" $ do
    let token = BcryptedAuthorization "foobar"
    actual <- runEnv env $ \c -> do
      userId <- insertToken c token
      fetchToken c userId
    actual `shouldBe` Just token

  it "insertGroceryItems then selectGroceryItems" $ do
    item <- liftIO $ generate arbitraryGroceryItem
    actual <- runEnv env $ \c -> do
      groceryItemIds <- insertGroceryItems c envUser [item]
      Map.elems <$> selectGroceryItems c envUser groceryItemIds
    orderedGroceryItemItem <$> actual `shouldMatchList` [item]

  it "insertOrderedGroceryItems then selectGroceryItems" $ do
    item <- liftIO $ generate arbitraryOrderedGroceryItem
    actual <- runEnv env $ \c -> do
      groceryItemIds <- insertOrderedGroceryItems c envUser [item]
      Map.elems <$> selectGroceryItems c envUser groceryItemIds
    actual `shouldMatchList` [item]

  it "updateGroceryItem" $ do
    item <- liftIO $ generate arbitraryGroceryItem
    before <- liftIO $ generate arbitraryGroceryItem
    after <- liftIO $ generate arbitraryGroceryItem
    actual <- runEnv env $ \c -> do
      groceryItemIds@([_, _, groceryItemId]) <- insertOrderedGroceryItems c envUser (uncurry OrderedGroceryItem <$> [(before, 1), (after, 2), (item, 3)])
      updateOrderedGroceryItem c envUser groceryItemId (OrderedGroceryItem item 2)
      Map.elems <$> selectGroceryItems c envUser groceryItemIds
    actual `shouldMatchList` uncurry OrderedGroceryItem <$> [(before, 1), (item, 2), (after, 3)]

  it "mergeGroceryItems" $ do
    item <- liftIO $ generate arbitraryOrderedGroceryItem
    items <- liftIO $ generate $ listOf1 arbitraryGroceryItem
    actual <- runEnv env $ \c -> do
      groceryItemIds <- insertGroceryItems c envUser items
      void $ mergeGroceryItems c envUser groceryItemIds item
      Map.elems <$> selectGroceryItems c envUser []
    actual `shouldMatchList` [item]

  it "deleteGroceryItems" $ do
    item <- liftIO $ generate arbitraryGroceryItem
    item2 <- liftIO $ generate arbitraryGroceryItem
    actual <- runEnv env $ \c -> do
      groceryItemId:_ <- insertGroceryItems c envUser [item, item2]
      deleteGroceryItems c envUser [groceryItemId]
      Map.elems <$> selectGroceryItems c envUser []
    orderedGroceryItemItem <$> actual `shouldMatchList` [item2]

  it "unmergeGroceryItems" $ do
    ingredient <- generate arbitraryIngredient
    ingredients <- generate $ listOf1 arbitraryIngredient
    let item = ingredientToGroceryItem True ingredient
    runEnv env $ \c -> do
      [groceryItemId] <- insertGroceryItems c envUser [item]
      ingredientId:_ <- insertGroceryItemIngredients c envUser ((groceryItemId,) <$> (ingredient:ingredients))
      unmergeGroceryItems c envUser [ingredientId]

  it "insertGroceryItemIngredients" $ do
    ingredient <- generate arbitraryIngredient
    let item = ingredientToGroceryItem True ingredient
    runEnv env $ \c -> do
      [groceryItemId] <- insertGroceryItems c envUser [item]
      void $ insertGroceryItemIngredients c envUser [(groceryItemId, ingredient)]

  it "insertRecipe then selectRecipes then selectIngredientsByRecipeId then selectRecipeIngredientIds" $ do
    recipe <- generate arbitraryRecipe
    ingredients <- generate $ listOf1 arbitraryIngredient
    let items = ingredientToGroceryItem True <$> ingredients
    (actualRecipes, actualIngredients, ingredientIds, actualIngredientIds) <- runEnv env $ \c -> do
      groceryItemIds <- insertGroceryItems c envUser items
      recipeId <- insertRecipe c envUser recipe (zip groceryItemIds ingredients)
      actualRecipes <- Map.elems <$> selectRecipes c envUser []
      (ingredientIds, actualIngredients) <- unzip . mapToList . map orderedIngredientIngredient <$> selectIngredientsByRecipeId c envUser recipeId
      actualIngredientIds <- selectRecipeIngredientIds c envUser [recipeId]
      pure (actualRecipes, actualIngredients, ingredientIds, actualIngredientIds)
    actualRecipes `shouldMatchList` [recipe]
    actualIngredients `shouldMatchList` ingredients
    actualIngredientIds `shouldMatchList` ingredientIds

  it "insertRecipe then deactivateRecipe then activateRecipe" $ do
    recipe <- generate arbitraryRecipe
    ingredients <- generate $ listOf1 arbitraryIngredient
    let items = ingredientToGroceryItem True <$> ingredients
    runEnv env $ \c -> do
      groceryItemIds <- insertGroceryItems c envUser items
      recipeId <- insertRecipe c envUser recipe (zip groceryItemIds ingredients)
      deactivateRecipe c envUser recipeId
      activateRecipe c envUser recipeId

  it "insertRecipe then deleteRecipes" $ do
    recipe <- generate arbitraryRecipe
    ingredients <- generate $ listOf1 arbitraryIngredient
    let items = ingredientToGroceryItem True <$> ingredients
    runEnv env $ \c -> do
      groceryItemIds <- insertGroceryItems c envUser items
      recipeId <- insertRecipe c envUser recipe (zip groceryItemIds ingredients)
      deleteRecipes c envUser [recipeId]

  it "deactivateEverything" $ do
    recipe <- generate arbitraryRecipe
    ingredients <- generate $ listOf1 arbitraryIngredient
    let items = ingredientToGroceryItem True <$> ingredients
    runEnv env $ \c -> do
      groceryItemIds <- insertGroceryItems c envUser items
      void $ insertRecipe c envUser recipe (zip groceryItemIds ingredients)
      deactivateEverything c envUser

  it "selectCachedRecipe" $ do
    name <- generate $ arbitraryRecipeName
    ingredients <- generate $ listOf1 arbitraryIngredient
    steps <- generate $ listOf1 arbitraryStep
    let link = RecipeLink "foo"
        ingredientInfo = ScrapeInfo (ScrapeName "fooI") inception
        stepInfo = ScrapeInfo (ScrapeName "fooS") inception
        recipe = ScrapedRecipe name ingredients steps
        info = ScrapedInfoIngredientStep ingredientInfo stepInfo
    actual <- runEnv env $ \c -> do
      repsertCachedRecipe c link recipe info
      selectCachedRecipe c link 1000
    actual `shouldBe` Just recipe

  it "repsertCachedRecipe" $ do
    name <- generate $ arbitraryRecipeName
    ingredients <- generate $ listOf1 arbitraryIngredient
    steps <- generate $ listOf1 arbitraryStep
    let link1 = RecipeLink "foo"
        link2 = RecipeLink "bar"
        ingredientInfo = ScrapeInfo (ScrapeName "fooI") inception
        stepInfo = ScrapeInfo (ScrapeName "fooS") inception
        recipe = ScrapedRecipe name ingredients steps
        info1 = ScrapedInfoIngredient ingredientInfo
        info2 = ScrapedInfoIngredientStep ingredientInfo stepInfo
    runEnv env $ \c -> do
      repsertCachedRecipe c link1 recipe info1
      repsertCachedRecipe c link2 recipe info2

  it "refreshCachedRecipes" $ do
    name <- generate $ arbitraryRecipeName
    ingredients <- generate $ listOf1 arbitraryIngredient
    steps <- generate $ listOf1 arbitraryStep
    let link1 = RecipeLink "foo"
        link2 = RecipeLink "bar"
        ingredientInfo = ScrapeInfo (ScrapeName "fooI") inception
        stepInfo = ScrapeInfo (ScrapeName "fooS") inception
        recipe = ScrapedRecipe name ingredients steps
        info = ScrapedInfoIngredientStep ingredientInfo stepInfo
    (actualInvalidTime, actualTooBig, actualValid) <- runEnv env $ \c -> do
      repsertCachedRecipe c link1 recipe info
      repsertCachedRecipe c link2 recipe info
      refreshCachedRecipes c 0 1000
      invalidTime <- selectCachedRecipe c link1 1000
      repsertCachedRecipe c link1 recipe info
      refreshCachedRecipes c 1000 1
      tooBig <- selectCachedRecipe c link2 1000
      valid <- selectCachedRecipe c link1 1000
      pure (invalidTime, tooBig, valid)
    actualInvalidTime `shouldBe` Nothing
    actualTooBig `shouldBe` Nothing
    actualValid `shouldBe` Just recipe

  it "invalidateCachedRecipes" $ do
    name <- generate $ arbitraryRecipeName
    ingredients <- generate $ listOf1 arbitraryIngredient
    steps <- generate $ listOf1 arbitraryStep
    let link1 = RecipeLink "foo"
        link2 = RecipeLink "bar"
        info1 = ScrapedInfoIngredient (ScrapeInfo (ScrapeName "foo") inception)
        info2 = ScrapedInfoIngredient (ScrapeInfo (ScrapeName "bar") inception)
        recipe = ScrapedRecipe name ingredients steps
    (actualInvalid, actualValid) <- runEnv env $ \c -> do
      repsertCachedRecipe c link1 recipe info1
      repsertCachedRecipe c link2 recipe info2
      invalidateCachedRecipes c ((==) info1)
      (,)
        <$> selectCachedRecipe c link1 1000
        <*> selectCachedRecipe c link2 1000
    actualInvalid `shouldBe` Nothing
    actualValid `shouldBe` Just recipe
