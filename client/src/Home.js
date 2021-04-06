import React, { useEffect } from "react";
import { connect } from "react-redux";
import { bindActionCreators } from "redux";
import _ from "lodash";
import {
  ListItemText,
  TextField,
  List,
  ListItem,
  Typography,
  ListItemSecondaryAction,
  Checkbox,
  IconButton,
  Grid,
} from "@material-ui/core";
import AddIcon from "@material-ui/icons/Add";
import InfoIcon from "@material-ui/icons/Info";
import DeleteIcon from "@material-ui/icons/Delete";

import store from "./store";
import * as Actions from "./actions";

const Home = ({
  ingredients,
  ingredientsDirty,
  newLink,
  recipes,
  includeAll,
  actions,
}) => {
  const renderedRecipeList = Object.keys(recipes).map((key, _) => (
    <ListItem key={`recipe-${key}`}>
      <ListItemText primary={recipes[key].name} />
      <IconButton edge="end" href={`recipe/${key}`}>
        <InfoIcon />
      </IconButton>
      <IconButton
        edge="end"
        onClick={() => store.dispatch(Actions.deleteRecipe(key))}
      >
        <DeleteIcon />
      </IconButton>
      <ListItemSecondaryAction>
        <Checkbox
          edge="end"
          onChange={(e) => actions.setRecipeChecked(key, e.target.checked)}
          checked={recipes[key].checked}
        />
      </ListItemSecondaryAction>
    </ListItem>
  ));
  const renderedIncludeAll = (
    <ListItem key="recipe-include-all">
      <ListItemText style={{ textAlign: "right" }} primary="Include All" />
      <ListItemSecondaryAction>
        <Checkbox
          edge="end"
          onChange={(e) => actions.setIncludeAll(e.target.checked)}
          checked={includeAll}
        />
      </ListItemSecondaryAction>
    </ListItem>
  );
  const renderedAddRecipe = (
    <ListItem key="recipe-add">
      <TextField
        id="standard-basic"
        fullWidth={true}
        label="Add Recipe"
        onBlur={(e) => actions.setNewLink(e.target.value)}
      />
      <IconButton
        onClick={() => store.dispatch(Actions.postNewRecipeLink(newLink))}
      >
        <AddIcon />
      </IconButton>
    </ListItem>
  );
  const renderedGroceryList = ingredients.map((x) => (
    <ListItem key={`ingredient-${x.name}`}>
      <ListItemText primary={Actions.renderIngredient(x)} />
    </ListItem>
  ));
  useEffect(() => {
    if (ingredientsDirty) {
      store.dispatch(Actions.resetIngredientsDirty());
      store.dispatch(
        Actions.fetchIngredients(
          _.reduce(
            recipes,
            (acc, value, key) => {
              if (value.checked) acc.push(key);
              return acc;
            },
            []
          )
        )
      );
    }
  });
  return (
    <Grid container spacing={3}>
      <Grid item xs={6}>
        <Typography variant="h5">Meals</Typography>
        {recipes ? (
          <List component="nav">
            {renderedIncludeAll}
            {renderedRecipeList}
            {renderedAddRecipe}
          </List>
        ) : (
          <Typography>Loading...</Typography>
        )}
      </Grid>
      <Grid item xs={6}>
        <Typography variant="h5">Grocery List</Typography>
        {ingredients ? (
          <div>
            <List>{renderedGroceryList}</List>
          </div>
        ) : (
          <Typography>Loading...</Typography>
        )}
      </Grid>
    </Grid>
  );
};

const mapStateToProps = (state) => ({
  ingredients: state.ingredients,
  newLink: state.newLink,
  recipes: state.recipes,
  includeAll: state.includeAll,
  ingredientsDirty: state.ingredientsDirty,
});

const mapDispatchToProps = (dispatch) => ({
  actions: bindActionCreators(Actions, dispatch),
});

export default connect(mapStateToProps, mapDispatchToProps)(Home);
