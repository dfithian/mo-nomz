import axios from "axios";

export const renderIngredient = ({ name, quantity, unit }) => {
  return `${quantity} ${unit} ${name}`;
};

export const fetchRecipes = () => {
  return async (dispatch) => {
    try {
      const recipes = await axios.get("/api/v1/recipe/list");
      dispatch(setRecipes(recipes.data));
    } catch (e) {
      console.log(e);
    }
  };
};

export const fetchIngredients = (recipeIds) => {
  return async (dispatch) => {
    try {
      const ingredients = await axios.get("/api/v1/ingredient/list", {
        params: { recipe: recipeIds },
      });
      dispatch(setIngredients(ingredients.data));
    } catch (e) {
      console.log(e);
    }
  };
};

export const postNewRecipeLink = (link) => {
  return async (dispatch) => {
    try {
      if (link) {
        await axios.post("/api/v1/recipe/import/link", { link });
        dispatch(setNewLink(""));
        dispatch(fetchRecipes());
      }
    } catch (e) {
      console.log(e);
    }
  };
};

export const deleteRecipe = (key) => {
  return async (dispatch) => {
    try {
      await axios.delete(`/api/v1/recipe/${key}`);
      dispatch(fetchRecipes());
      dispatch(fetchIngredients());
    } catch (e) {
      console.log(e);
    }
  };
};

export const setRecipes = (payload) => ({
  type: "UPDATE_RECIPES",
  recipes: payload.recipes,
});

export const setIngredients = (payload) => ({
  type: "UPDATE_INGREDIENTS",
  ingredients: payload.ingredients,
});

export const setNewLink = (payload) => ({
  type: "SET_NEW_LINK",
  newLink: payload,
});

export const setRecipeChecked = (key, value) => ({
  type: "SET_RECIPE_CHECKED",
  key,
  value,
});

export const setIncludeAll = (value) => ({
  type: "SET_INCLUDE_ALL",
  value,
});

export const resetIngredientsDirty = () => ({
  type: "RESET_INGREDIENTS_DIRTY",
});
