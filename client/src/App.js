import React from "react";
import {
  BrowserRouter as Router,
  Route,
  Redirect,
  Switch,
} from "react-router-dom";
import { Breadcrumbs, Link } from "@material-ui/core";

import Home from "./Home";
import Recipe from "./Recipe";

export default function App() {
  return (
    <Router>
      <div>
        <Breadcrumbs aria-label="breadcrumb">
          <Link href="/home">Home</Link>
        </Breadcrumbs>

        <hr />

        <Switch>
          <Route exact path="/">
            <Redirect to="home" />
          </Route>
          <Route exact strict path="/home" component={Home} />
          <Route path="/recipe/:id" component={Recipe} />
        </Switch>
      </div>
    </Router>
  );
}
