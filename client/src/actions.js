import axios from "axios";

export const renderIngredient = ({ name, quantity, unit }) => {
  return `${quantity} ${unit} ${name}`;
};

export const createUser = (username) => {
  return async (dispatch) => {
    try {
      if (username) {
        const userId = await axios.post("/api/v1/user", { username });
        dispatch(fetchUsers());
        dispatch(fetchRecipes(userId.data.userId));
        dispatch(fetchIngredients(userId.data.userId, []));
        dispatch(setUser(userId.data));
      }
    } catch (e) {
      console.log(e);
    }
  };
};

export const fetchUsers = () => {
  return async (dispatch) => {
    try {
      const users = await axios.get("/api/v1/user/list");
      dispatch(setUsers(users.data));
    } catch (e) {
      console.log(e);
    }
  };
};

export const fetchRecipes = (userId) => {
  return async (dispatch) => {
    try {
      const recipes = await axios.get(`/api/v1/user/${userId}/recipe/list`);
      dispatch(setRecipes(recipes.data));
    } catch (e) {
      console.log(e);
    }
  };
};

export const fetchIngredients = (userId, recipeIds) => {
  return async (dispatch) => {
    try {
      const ingredients = await axios.get(
        `/api/v1/user/${userId}/ingredient/list`,
        {
          params: { recipe: recipeIds },
        }
      );
      dispatch(setIngredients(ingredients.data));
    } catch (e) {
      console.log(e);
    }
  };
};

export const postNewRecipeLink = (userId, link) => {
  return async (dispatch) => {
    try {
      if (link) {
        await axios.post(`/api/v1/user/${userId}/recipe/import/link`, { link });
        dispatch(setNewLink(""));
        dispatch(fetchRecipes(userId));
        dispatch(fetchIngredients(userId, []));
      }
    } catch (e) {
      console.log(e);
    }
  };
};

export const deleteRecipe = (userId, key) => {
  return async (dispatch) => {
    try {
      await axios.delete(`/api/v1/user/${userId}/recipe/${key}`);
      dispatch(fetchRecipes(userId));
      dispatch(fetchIngredients(userId));
    } catch (e) {
      console.log(e);
    }
  };
};

export const setUsername = (payload) => ({
  type: "SET_USERNAME",
  newUsername: payload,
});

export const setUser = (payload) => ({
  type: "REDIRECT",
  redirect: `/user/${payload.userId}`,
});

export const setUsers = (payload) => ({
  type: "UPDATE_USERS",
  users: payload.users,
});

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
