module.exports = {
  siteMetadata: {
    title: "Grocer EZ",
    siteUrl: "https://mo-nomz.herokuapp.com"
  },
  plugins: [
    "gatsby-transformer-remark",
    {
      resolve: "gatsby-source-filesystem",
      options: {
        name: "pages",
        path: "./src/pages/",
      },
      __key: "pages",
    },
    "gatsby-plugin-sitemap",
  ],
};
