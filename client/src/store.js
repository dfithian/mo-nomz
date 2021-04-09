import { applyMiddleware, compose, createStore } from "redux";
import logger from "redux-logger";
import thunk from "redux-thunk";
import _ from "lodash";

const defaultState = {
  newUsername: "",
  users: {},
  usersFetched: false,
  recipes: {},
  recipesFetched: false,
  ingredients: [],
  ingredientsFetched: false,
  newLink: "",
  includeAll: false,
  ingredientsDirty: false,
};

const mainReducer = function (state = defaultState, action) {
  switch (action.type) {
    case "REDIRECT":
      return {
        ...state,
        redirect: action.redirect,
      };
    case "SET_USERNAME":
      return {
        ...state,
        newUsername: action.newUsername,
      };
    case "UPDATE_USERS":
      return {
        ...state,
        users: action.users,
        usersFetched: true,
      };
    case "UPDATE_RECIPES":
      const recipes = action.recipes;
      _.map(recipes, (r) => (r.checked = true));
      return {
        ...state,
        recipes,
        recipesFetched: true,
        includeAll: true,
      };
    case "UPDATE_INGREDIENTS":
      return {
        ...state,
        ingredients: action.ingredients,
        ingredientsFetched: true,
      };
    case "SET_NEW_LINK":
      return {
        ...state,
        newLink: action.newLink,
      };
    case "SET_RECIPE_CHECKED":
      const newRecipes = state.recipes;
      newRecipes[action.key].checked = action.value;
      return {
        ...state,
        recipes: {
          ...newRecipes,
        },
        ingredientsDirty: true,
      };
    case "SET_INCLUDE_ALL":
      const allNewRecipes = state.recipes;
      _.map(allNewRecipes, (r) => (r.checked = action.value));
      return {
        ...state,
        recipes: {
          ...allNewRecipes,
        },
        includeAll: action.value,
        ingredientsDirty: true,
      };
    case "RESET_INGREDIENTS_DIRTY":
      return {
        ...state,
        ingredientsDirty: false,
      };
    default:
      return state;
  }
};

export default createStore(
  mainReducer,
  defaultState,
  compose(applyMiddleware(thunk, logger))
);
