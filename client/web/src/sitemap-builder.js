import Generator from 'react-router-sitemap-generator';
import router from './sitemap-router';

const generator = new Generator(
  'https://mo-nomz.herokuapp.com',
  router,
  {
    lastmod: new Date().toISOString().slice(0, 10),
    changefreq: 'monthly',
    priority: 0.8,
  }
)

generator.save('public/sitemap.xml')
