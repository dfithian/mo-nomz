import React from "react";
import { connect } from "react-redux";
import { bindActionCreators } from "redux";
import { Redirect } from "react-router";
import {
  TextField,
  IconButton,
  List,
  ListItem,
  Link,
  Typography,
} from "@material-ui/core";
import LockOpenIcon from "@material-ui/icons/LockOpen";

import store from "./store";
import * as Actions from "./actions";

const Home = ({ redirect, users, usersFetched, newUsername, actions }) => {
  const renderedUsers = Object.keys(users).map((key, _) => (
    <ListItem key={`user-${key}`}>
      <Link href={`/user/${key}`}>
        <Typography>{users[key].username}</Typography>
      </Link>
    </ListItem>
  ));
  const renderedLoading = () => {
    store.dispatch(Actions.fetchUsers());
    return (
      <ListItem key="user-loading">
        <Typography>Loading...</Typography>
      </ListItem>
    );
  };
  if (redirect) {
    return <Redirect to={redirect} />;
  } else {
    return (
      <div>
        <Typography variant="h5">Select or create user</Typography>
        <List component="nav">
          {usersFetched ? renderedUsers : renderedLoading()}
          <ListItem key="user-add">
            <TextField
              id="standard-basic"
              label="New user"
              onBlur={(e) => actions.setUsername(e.target.value)}
            />
            <IconButton
              onClick={() => store.dispatch(Actions.createUser(newUsername))}
            >
              <LockOpenIcon />
            </IconButton>
          </ListItem>
        </List>
      </div>
    );
  }
};

const mapStateToProps = (state) => ({
  users: state.users,
  usersFetched: state.usersFetched,
  newUsername: state.newUsername,
  redirect: state.redirect,
});

const mapDispatchToProps = (dispatch) => ({
  actions: bindActionCreators(Actions, dispatch),
});

export default connect(mapStateToProps, mapDispatchToProps)(Home);
