import * as React from "react"
import appStore from "../images/app-store.svg"
import groceryList from "../images/grocery-list.png"
import recipeList from "../images/recipe-list.png"
import addLink from "../images/add-link.png"
import addBulk from "../images/add-bulk.png"

const pageStyles = {
  color: "#232129",
  padding: 25,
  fontFamily: "-apple-system, Roboto, sans-serif, serif",
}
const inlineStyles = {
  marginRight: 25,
  marginLeft: 25,
}
const centerStyles = {
  textAlign: "center",
}
const imgStyles = {
  padding: 5,
}

const IndexPage = () => {
  return (
    <main className="center" style={pageStyles}>
      <title>Nomz</title>
      <h1>Nomz</h1>
      <p>Meal Planner & Grocery List</p>
      <div style={centerStyles}>
        <img style={imgStyles} src={groceryList} alt="Grocery List" width="150"/>
        <img style={imgStyles} src={recipeList} alt="Recipe List" width="150"/>
        <img style={imgStyles} src={addLink} alt="Add Link" width="150"/>
        <img style={imgStyles} src={addBulk} alt="Add Bulk" width="150"/>
      </div>
      <div>
        <a style={inlineStyles} href="https://venmo.com/code?user_id=1305125788319744156">❤️ Donate</a>
        <a style={inlineStyles} href="/privacy-policy">Privacy Policy</a>
        <a style={inlineStyles} href="/support">Support</a>
        <a style={inlineStyles} href="https://dfithian.github.io/2021/06/08/nomz-meal-planner.html">Blog</a>
      </div>
      <div>
        <a href="https://apps.apple.com/us/app/grocer-ez/id1563273742">
          <img style={imgStyles} src={appStore} alt="Download on the App Store" />
        </a>
      </div>
    </main>
  )
}

export default IndexPage
