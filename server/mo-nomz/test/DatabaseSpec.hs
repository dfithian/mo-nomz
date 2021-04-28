module DatabaseSpec where

import ClassyPrelude

import Test.Hspec (Spec, describe, it, shouldBe, shouldMatchList)
import Test.QuickCheck (generate, listOf1)
import qualified Data.Map as Map

import Auth (BcryptedAuthorization(..))
import Gen (arbitraryGroceryItem, arbitraryIngredient, arbitraryRecipe)
import TestEnv (Env(..), runEnv)
import Types (ingredientToGroceryItem)

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
    actual `shouldMatchList` [item]

  it "mergeGroceryItems" $ do
    item <- liftIO $ generate arbitraryGroceryItem
    items <- liftIO $ generate $ listOf1 arbitraryGroceryItem
    actual <- runEnv env $ \c -> do
      groceryItemIds <- insertGroceryItems c envUser (item:items)
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
    actual `shouldMatchList` [item2]

  it "unmergeGroceryItems" $ do
    ingredient <- generate arbitraryIngredient
    ingredients <- generate $ listOf1 arbitraryIngredient
    let item = ingredientToGroceryItem ingredient
    runEnv env $ \c -> do
      [groceryItemId] <- insertGroceryItems c envUser [item]
      ingredientId:_ <- insertGroceryItemIngredients c envUser ((groceryItemId,) <$> (ingredient:ingredients))
      unmergeGroceryItems c envUser [ingredientId]

  it "insertGroceryItemIngredients" $ do
    ingredient <- generate arbitraryIngredient
    let item = ingredientToGroceryItem ingredient
    runEnv env $ \c -> do
      [groceryItemId] <- insertGroceryItems c envUser [item]
      void $ insertGroceryItemIngredients c envUser [(groceryItemId, ingredient)]

  it "insertRecipe then selectRecipes then selectIngredientsByRecipeId then selectRecipeIngredientIds" $ do
    recipe <- generate arbitraryRecipe
    ingredients <- generate $ listOf1 arbitraryIngredient
    let items = ingredientToGroceryItem <$> ingredients
    (actualRecipes, actualIngredients, ingredientIds, actualIngredientIds) <- runEnv env $ \c -> do
      groceryItemIds <- insertGroceryItems c envUser items
      recipeId <- insertRecipe c envUser recipe (zip groceryItemIds ingredients)
      actualRecipes <- Map.elems <$> selectRecipes c envUser []
      (ingredientIds, actualIngredients) <- unzip <$> selectIngredientsByRecipeId c envUser recipeId
      actualIngredientIds <- selectRecipeIngredientIds c envUser [recipeId]
      pure (actualRecipes, actualIngredients, ingredientIds, actualIngredientIds)
    actualRecipes `shouldMatchList` [recipe]
    actualIngredients `shouldMatchList` ingredients
    actualIngredientIds `shouldMatchList` ingredientIds

  it "insertRecipe then deactivateRecipe then activateRecipe" $ do
    recipe <- generate arbitraryRecipe
    ingredients <- generate $ listOf1 arbitraryIngredient
    let items = ingredientToGroceryItem <$> ingredients
    runEnv env $ \c -> do
      groceryItemIds <- insertGroceryItems c envUser items
      recipeId <- insertRecipe c envUser recipe (zip groceryItemIds ingredients)
      deactivateRecipe c envUser recipeId
      activateRecipe c envUser recipeId

  it "insertRecipe then deleteRecipes" $ do
    recipe <- generate arbitraryRecipe
    ingredients <- generate $ listOf1 arbitraryIngredient
    let items = ingredientToGroceryItem <$> ingredients
    runEnv env $ \c -> do
      groceryItemIds <- insertGroceryItems c envUser items
      recipeId <- insertRecipe c envUser recipe (zip groceryItemIds ingredients)
      deleteRecipes c envUser [recipeId]

  it "deactivateEverything" $ do
    recipe <- generate arbitraryRecipe
    ingredients <- generate $ listOf1 arbitraryIngredient
    let items = ingredientToGroceryItem <$> ingredients
    runEnv env $ \c -> do
      groceryItemIds <- insertGroceryItems c envUser items
      void $ insertRecipe c envUser recipe (zip groceryItemIds ingredients)
      deactivateEverything c envUser
