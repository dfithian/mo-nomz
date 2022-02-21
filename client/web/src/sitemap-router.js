import React from 'react';
import { Route } from 'react-router-dom';

const router = (
  <Route>
    <Route path="/" />
    <Route path="/#/status" />
    <Route path="/#/privacy-policy" />
    <Route path="/#/support" />
    <Route path="/#/recipe" />
    <Route path="/#/not-found" />
  </Route>
)

export default router
