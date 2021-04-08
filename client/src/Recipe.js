import React from "react";
import { connect } from "react-redux";
import { useParams } from "react-router";
import {
  Breadcrumbs,
  IconButton,
  Link,
  List,
  ListItem,
  ListItemText,
  Typography,
} from "@material-ui/core";
import DeleteIcon from "@material-ui/icons/Delete";
import OpenInNewIcon from "@material-ui/icons/OpenInNew";

import store from "./store";
import * as Actions from "./actions";

const Recipe = ({ recipes }) => {
  const { userId, recipeId } = useParams();
  if (recipes && recipes[recipeId]) {
    const { name, ingredients, link } = recipes[recipeId];
    return (
      <div>
        <Breadcrumbs>
          <Link href="/">
            <Typography>Home</Typography>
          </Link>
          <Link href={`/user/${userId}`}>
            <Typography>Recipes</Typography>
          </Link>
          <Link href={`/user/${userId}/recipe/${recipeId}`}>
            <Typography>{name}</Typography>
          </Link>
        </Breadcrumbs>

        <br />

        <Typography variant="h5">
          {name}
          {link ? (
            <IconButton href={link} target="_blank">
              <OpenInNewIcon />
            </IconButton>
          ) : null}
          <IconButton
            edge="end"
            onClick={() =>
              store.dispatch(Actions.deleteRecipe(userId, recipeId))
            }
          >
            <DeleteIcon />
          </IconButton>
        </Typography>
        <List>
          {ingredients.map((x) => (
            <ListItem key={x.name}>
              <ListItemText primary={Actions.renderIngredient(x)} />
            </ListItem>
          ))}
        </List>
      </div>
    );
  } else {
    store.dispatch(Actions.fetchRecipes(userId));
    return <Typography>Loading...</Typography>;
  }
};

const mapStateToProps = (state) => ({
  recipes: state.recipes,
});

export default connect(mapStateToProps)(Recipe);
