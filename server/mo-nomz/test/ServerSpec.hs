module ServerSpec where

import ClassyPrelude

import Test.Hspec (Spec, describe, it, shouldMatchList, before)
import Test.QuickCheck (generate)

import API.Types (DeleteRecipeRequest(..), MergeGroceryItemRequest(..), DeleteGroceryItemRequest(..), UpdateRecipeRequest(..), ListGroceryItemResponse(..), ReadableGroceryItemAggregate(..))
import Conversion (mkReadableQuantity, mkReadableUnit, mkReadableGroceryItem)
import Gen (arbitraryIngredient, arbitraryRecipe)
import TestEnv (Env(..), runEnv, runServer)
import Types (ingredientToGroceryItem, Ingredient(..), Quantity(..))
import qualified Database

import Server

spec :: Env -> Spec
spec env@Env {..} = describe "Server" $ do
  let runBefore = do
        (recipe1, recipe2, ingredient1, ingredient2, ingredient3, ingredient4, ingredient5, ingredient6) <- generate $ (,,,,,,,)
          <$> arbitraryRecipe
          <*> arbitraryRecipe
          <*> arbitraryIngredient
          <*> arbitraryIngredient
          <*> arbitraryIngredient
          <*> arbitraryIngredient
          <*> arbitraryIngredient
          <*> arbitraryIngredient
        runEnv env $ \c -> do
          [groceryItemId1, groceryItemId2, groceryItemId3, groceryItemId4, groceryItemId5, groceryItemId6] <-
            Database.insertGroceryItems c envUser (ingredientToGroceryItem <$> [ingredient1, ingredient2, ingredient3, ingredient4, ingredient5, ingredient6])
          recipeId1 <- Database.insertRecipe c envUser recipe1 [(groceryItemId1, ingredient1), (groceryItemId2, ingredient2)]
          recipeId2 <- Database.insertRecipe c envUser recipe2 [(groceryItemId3, ingredient3), (groceryItemId4, ingredient4)]
          void $ Database.insertGroceryItemIngredients c envUser [(groceryItemId5, ingredient5), (groceryItemId6, ingredient6)]
          Database.automergeGroceryItems c envUser
          pure
            ( ( recipeId1, recipeId2 )
            , ( recipe1, recipe2 )
            , ( groceryItemId1, groceryItemId2, groceryItemId3, groceryItemId4, groceryItemId5, groceryItemId6 )
            , ( ingredient1, ingredient2, ingredient3, ingredient4, ingredient5, ingredient6 )
            )

  before runBefore $ do
    it "insert, merge" $ \(_, _, groceryItemIds, ingredients) -> do
      let (groceryItemId1, _, groceryItemId3, _, groceryItemId5, _) = groceryItemIds
          (ingredient1, ingredient2, _, ingredient4, _, ingredient6) = ingredients
      ListGroceryItemResponse items <- runServer env $ do
        void $ postMergeGroceryItem envAuth envUser MergeGroceryItemRequest
          { mergeGroceryItemRequestIds = setFromList [groceryItemId1, groceryItemId3, groceryItemId5]
          , mergeGroceryItemRequestName = ingredientName ingredient1
          , mergeGroceryItemRequestQuantity = mkReadableQuantity $ ingredientQuantity ingredient1
          , mergeGroceryItemRequestUnit = mkReadableUnit $ ingredientUnit ingredient1
          , mergeGroceryItemRequestActive = True
          }
        getGroceryItems envAuth envUser
      readableGroceryItemAggregateItem <$> items `shouldMatchList` mkReadableGroceryItem . ingredientToGroceryItem <$> [ingredient1, ingredient2, ingredient4, ingredient6]

    it "insert, merge, delete grocery item" $ \(_, _, groceryItemIds, ingredients) -> do
      let (groceryItemId1, groceryItemId2, groceryItemId3, _, groceryItemId5, _) = groceryItemIds
          (ingredient1, _, _, ingredient4, _, ingredient6) = ingredients
      mergedGroceryItemId <- runEnv env $ \c ->
        Database.mergeGroceryItems c envUser [groceryItemId1, groceryItemId3, groceryItemId5] (ingredientToGroceryItem ingredient1)
      ListGroceryItemResponse items <- runServer env $ do
        void $ deleteGroceryItem envAuth envUser DeleteGroceryItemRequest
          { deleteGroceryItemRequestIds = setFromList [mergedGroceryItemId, groceryItemId2]
          }
        getGroceryItems envAuth envUser
      readableGroceryItemAggregateItem <$> items `shouldMatchList` mkReadableGroceryItem . ingredientToGroceryItem <$> [ingredient4, ingredient6]

    it "insert, merge, delete recipe" $ \(recipeIds, _, groceryItemIds, ingredients) -> do
      let (recipeId1, _) = recipeIds
          (groceryItemId1, groceryItemId2, groceryItemId3, _, groceryItemId5, _) = groceryItemIds
          (ingredient1, _, ingredient3, ingredient4, ingredient5, ingredient6) = ingredients
          name = ingredientName ingredient1
          expected = [ingredient4, ingredient6] <> (
            case ingredientUnit ingredient3 == ingredientUnit ingredient5 of
              True -> [ingredient3 { ingredientName = name, ingredientQuantity = ingredientQuantity ingredient3 + ingredientQuantity ingredient5 }]
              False -> [ingredient3 { ingredientName = name }, ingredient5 { ingredientName = name }]
            )
      void $ runEnv env $ \c ->
        Database.mergeGroceryItems c envUser [groceryItemId1, groceryItemId3, groceryItemId5] (ingredientToGroceryItem ingredient1)
      ListGroceryItemResponse items <- runServer env $ do
        void $ deleteGroceryItem envAuth envUser DeleteGroceryItemRequest
          { deleteGroceryItemRequestIds = setFromList [groceryItemId2]
          }
        void $ deleteRecipes envAuth envUser DeleteRecipeRequest
          { deleteRecipeRequestIds = setFromList [recipeId1]
          }
        getGroceryItems envAuth envUser
      readableGroceryItemAggregateItem <$> items `shouldMatchList` mkReadableGroceryItem . ingredientToGroceryItem <$> expected

    it "insert, merge, deactivate recipe" $ \(recipeIds, _, groceryItemIds, ingredients) -> do
      let (recipeId1, _) = recipeIds
          (groceryItemId1, groceryItemId2, groceryItemId3, _, groceryItemId5, _) = groceryItemIds
          (ingredient1, _, ingredient3, ingredient4, ingredient5, ingredient6) = ingredients
          name = ingredientName ingredient1
          expected = [ingredient4, ingredient6] <> (
            case ingredientUnit ingredient3 == ingredientUnit ingredient5 of
              True -> [ingredient3 { ingredientName = name, ingredientQuantity = ingredientQuantity ingredient3 + ingredientQuantity ingredient5 }]
              False -> [ingredient3 { ingredientName = name }, ingredient5 { ingredientName = name }]
            )
      void $ runEnv env $ \c ->
        Database.mergeGroceryItems c envUser [groceryItemId1, groceryItemId3, groceryItemId5] (ingredientToGroceryItem ingredient1)
      ListGroceryItemResponse items <- runServer env $ do
        void $ deleteGroceryItem envAuth envUser DeleteGroceryItemRequest
          { deleteGroceryItemRequestIds = setFromList [groceryItemId2]
          }
        void $ postUpdateRecipe envAuth envUser UpdateRecipeRequest
          { updateRecipeRequestId = recipeId1
          , updateRecipeRequestActive = False
          }
        getGroceryItems envAuth envUser
      readableGroceryItemAggregateItem <$> items `shouldMatchList` mkReadableGroceryItem . ingredientToGroceryItem <$> expected

    it "insert, merge, deactivate everything" $ \(recipeIds, _, groceryItemIds, ingredients) -> do
      let (recipeId1, _) = recipeIds
          (groceryItemId1, groceryItemId2, groceryItemId3, _, groceryItemId5, _) = groceryItemIds
          (ingredient1, _, _, _, _, _) = ingredients
      void $ runEnv env $ \c ->
        Database.mergeGroceryItems c envUser [groceryItemId1, groceryItemId3, groceryItemId5] (ingredientToGroceryItem ingredient1)
      ListGroceryItemResponse items <- runServer env $ do
        void $ deleteGroceryItem envAuth envUser DeleteGroceryItemRequest
          { deleteGroceryItemRequestIds = setFromList [groceryItemId2]
          }
        void $ postUpdateRecipe envAuth envUser UpdateRecipeRequest
          { updateRecipeRequestId = recipeId1
          , updateRecipeRequestActive = False
          }
        void $ postClearGroceryItems envAuth envUser
        getGroceryItems envAuth envUser
      readableGroceryItemAggregateItem <$> items `shouldMatchList` []

    it "insert, merge, deactivate everything, insert and merge again" $ \(recipeIds, recipes, groceryItemIds, ingredients) -> do
      let (recipeId1, _) = recipeIds
          (recipe1, recipe2) = recipes
          (groceryItemId1, groceryItemId2, groceryItemId3, _, groceryItemId5, _) = groceryItemIds
          (ingredient1, ingredient2, ingredient3, _, ingredient5, _) = ingredients
      void $ runEnv env $ \c ->
        Database.mergeGroceryItems c envUser [groceryItemId1, groceryItemId3, groceryItemId5] (ingredientToGroceryItem ingredient1)
      runServer env $ do
        void $ deleteGroceryItem envAuth envUser DeleteGroceryItemRequest
          { deleteGroceryItemRequestIds = setFromList [groceryItemId2]
          }
        void $ postUpdateRecipe envAuth envUser UpdateRecipeRequest
          { updateRecipeRequestId = recipeId1
          , updateRecipeRequestActive = False
          }
        void $ postClearGroceryItems envAuth envUser
        void $ postUpdateRecipe envAuth envUser UpdateRecipeRequest
          { updateRecipeRequestId = recipeId1
          , updateRecipeRequestActive = True
          }
      runEnv env $ \c -> do
        [groceryItemId7, groceryItemId8, groceryItemId9] <-
          Database.insertGroceryItems c envUser (ingredientToGroceryItem <$> [ingredient1, ingredient3, ingredient5])
        void $ Database.insertRecipe c envUser recipe1 [(groceryItemId7, ingredient1)]
        void $ Database.insertRecipe c envUser recipe2 [(groceryItemId8, ingredient3)]
        void $ Database.insertGroceryItemIngredients c envUser [(groceryItemId9, ingredient5)]
        Database.automergeGroceryItems c envUser
      ListGroceryItemResponse items <- runServer env $ do
        getGroceryItems envAuth envUser

      let expected = map (mkReadableGroceryItem . ingredientToGroceryItem) $
            [ ingredient1
                { ingredientQuantity = case ingredientQuantity ingredient1 of
                    QuantityMissing -> QuantityMissing
                    Quantity x -> Quantity $ 2 * x
                }
            , ingredient2
            , ingredient3
            , ingredient5
            ]
      readableGroceryItemAggregateItem <$> items `shouldMatchList` expected
