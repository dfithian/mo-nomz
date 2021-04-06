import React from "react";
import { connect } from "react-redux";
import { useParams } from "react-router";
import { bindActionCreators } from "redux";
import {
  IconButton,
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
  const { id } = useParams();
  if (recipes && recipes[id]) {
    const { name, ingredients, link } = recipes[id];
    return (
      <div>
        <Typography variant="h5">
          {name}
          {link ? (
            <IconButton href={link} target="_blank">
              <OpenInNewIcon />
            </IconButton>
          ) : null}
          <IconButton
            edge="end"
            onClick={() => store.dispatch(Actions.deleteRecipe(id))}
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
    return (
      <div>
        <Typography>Loading...</Typography>
      </div>
    );
  }
};

const mapStateToProps = (state) => ({
  recipes: state.recipes,
});

const mapDispatchToProps = (dispatch) => ({
  actions: bindActionCreators(Actions, dispatch),
});

export default connect(mapStateToProps, mapDispatchToProps)(Recipe);
