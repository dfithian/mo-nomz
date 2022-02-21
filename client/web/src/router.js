import React from 'react';
import { HashRouter, Routes, Route } from 'react-router-dom';
import HomePage from './routes/home';
import NotFoundPage from './routes/404';
import PrivacyPolicyPage from './routes/privacy-policy';
import SupportPage from './routes/support';
import RecipePage from './routes/recipe';
import './styles/global.scss';

const router = (
  <HashRouter basename="/">
    <Routes>
      <Route path="/" element={<HomePage />} />
      <Route path="/privacy-policy" element={<PrivacyPolicyPage />} />
      <Route path="/support" element={<SupportPage />} />
      <Route path="/recipe" element={<RecipePage />} />
      <Route path="/not-found" element={<NotFoundPage />} />
      <Route path="*" element={<NotFoundPage />} />
    </Routes>
  </HashRouter>
)

export default router
