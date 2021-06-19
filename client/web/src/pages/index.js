import * as React from "react"
import appStore from "../images/app-store.svg"

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
      <h2>Meal Planner & Grocery List</h2>
      <div className="container" style={centerStyles}>
        <div className="item">
          <p>Add a recipe link</p>
          <img style={imgStyles} src="/add-link.gif" alt="Add Link" width="150"/>
        </div>
        <div className="item">
          <p>Add groceries</p>
          <img style={imgStyles} src="/add-manual.gif" alt="Add Manual" width="150"/>
        </div>
        <div className="item">
          <p>Reorder items</p>
          <img style={imgStyles} src="/reorder.gif" alt="Reorder" width="150"/>
        </div>
        <div className="item">
          <p>Activate a saved recipe</p>
          <img style={imgStyles} src="/activate.gif" alt="Activate" width="150"/>
        </div>
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
