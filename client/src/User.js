import React, { useEffect } from "react";
import { connect } from "react-redux";
import { useParams } from "react-router";
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
  Breadcrumbs,
  Link,
} from "@material-ui/core";
import AddIcon from "@material-ui/icons/Add";
import InfoIcon from "@material-ui/icons/Info";
import DeleteIcon from "@material-ui/icons/Delete";

import store from "./store";
import * as Actions from "./actions";

const User = ({
  ingredients,
  ingredientsDirty,
  newLink,
  recipes,
  includeAll,
  actions,
}) => {
  const { userId } = useParams();
  useEffect(() => {
    if (ingredientsDirty) {
      store.dispatch(Actions.resetIngredientsDirty());
      store.dispatch(
        Actions.fetchIngredients(
          userId,
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
  if (!_.isEmpty(recipes) && !_.isEmpty(ingredients)) {
    return (
      <div>
        <Breadcrumbs>
          <Link href="/">
            <Typography>Home</Typography>
          </Link>
          <Link href={`/user/${userId}`}>
            <Typography>Recipes</Typography>
          </Link>
        </Breadcrumbs>

        <br />

        <Grid container spacing={3}>
          <Grid item xs={6}>
            <Typography variant="h5">Meals</Typography>
            <List component="nav">
              <ListItem key="recipe-include-all">
                <ListItemText
                  style={{ textAlign: "right" }}
                  primary="Include in grocery list"
                />
                <ListItemSecondaryAction>
                  <Checkbox
                    edge="end"
                    onChange={(e) => actions.setIncludeAll(e.target.checked)}
                    checked={includeAll}
                  />
                </ListItemSecondaryAction>
              </ListItem>
              {Object.keys(recipes).map((key, _) => (
                <ListItem key={`recipe-${key}`}>
                  <ListItemText primary={recipes[key].name} />
                  <IconButton edge="end" href={`/user/${userId}/recipe/${key}`}>
                    <InfoIcon />
                  </IconButton>
                  <IconButton
                    edge="end"
                    onClick={() =>
                      store.dispatch(Actions.deleteRecipe(userId, key))
                    }
                  >
                    <DeleteIcon />
                  </IconButton>
                  <ListItemSecondaryAction>
                    <Checkbox
                      edge="end"
                      onChange={(e) =>
                        actions.setRecipeChecked(key, e.target.checked)
                      }
                      checked={recipes[key].checked}
                    />
                  </ListItemSecondaryAction>
                </ListItem>
              ))}
              <ListItem key="recipe-add">
                <TextField
                  id="standard-basic"
                  fullWidth={true}
                  label="Add Recipe"
                  onBlur={(e) => actions.setNewLink(e.target.value)}
                />
                <IconButton
                  onClick={() =>
                    store.dispatch(Actions.postNewRecipeLink(userId, newLink))
                  }
                >
                  <AddIcon />
                </IconButton>
              </ListItem>
            </List>
          </Grid>
          <Grid item xs={6}>
            <Typography variant="h5">Grocery List</Typography>
            <List>
              {ingredients.map((x) => (
                <ListItem key={`ingredient-${x.name}`}>
                  <ListItemText primary={Actions.renderIngredient(x)} />
                </ListItem>
              ))}
            </List>
          </Grid>
        </Grid>
      </div>
    );
  } else {
    store.dispatch(Actions.fetchRecipes(userId));
    store.dispatch(Actions.fetchIngredients(userId, []));
    return <Typography>Loading...</Typography>;
  }
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

export default connect(mapStateToProps, mapDispatchToProps)(User);
