module API.Types where

import ClassyPrelude

import Data.Aeson.TH (deriveJSON)

import Json (jsonOptions)
import Types (Ingredient, Recipe, RecipeId, RecipeName)

data RecipeImportLinkRequest = RecipeImportLinkRequest
  { recipeImportLinkRequestLink :: Text
  }
  deriving (Eq, Ord, Show)

data RecipeImportBodyRequest = RecipeImportBodyRequest
  { recipeImportBodyRequestName        :: RecipeName
  , recipeImportBodyRequestIngredients :: [Ingredient]
  }
  deriving (Eq, Ord, Show)

data RecipeImportResponse = RecipeImportResponse
  { recipeImportResponseId :: RecipeId
  }
  deriving (Eq, Ord, Show)

data ListRecipeResponse = ListRecipeResponse
  { listRecipeResponseRecipes :: Map RecipeId Recipe
  }
  deriving (Eq, Ord, Show)

data ListIngredientResponse = ListIngredientResponse
  { listIngredientResponseIngredients :: [Ingredient]
  }
  deriving (Eq, Ord, Show)

deriveJSON (jsonOptions "recipeImportLinkRequest") ''RecipeImportLinkRequest
deriveJSON (jsonOptions "recipeImportBodyRequest") ''RecipeImportBodyRequest
deriveJSON (jsonOptions "recipeImportResponse") ''RecipeImportResponse
deriveJSON (jsonOptions "listRecipeResponse") ''ListRecipeResponse
deriveJSON (jsonOptions "listIngredientResponse") ''ListIngredientResponse
