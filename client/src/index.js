import React from "react";
import ReactDOM from "react-dom";
import { Route, Switch } from "react-router";
import { BrowserRouter as Router } from "react-router-dom";
import { Provider } from "react-redux";

import store from "./store";
import Home from "./Home";
import User from "./User";
import Recipe from "./Recipe";

ReactDOM.render(
  <Provider store={store}>
    <Router>
      <Switch>
        <Route exact path="/" component={Home} />
        <Route exact path="/user/:userId" component={User} />
        <Route exact path="/user/:userId/recipe/:recipeId" component={Recipe} />
      </Switch>
    </Router>
  </Provider>,
  document.getElementById("root")
);
