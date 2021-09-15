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
      <div>
        <a style={inlineStyles} href="https://venmo.com/code?user_id=1305125788319744156"><span role="img" aria-label="Heart">❤</span>️ Donate</a>
        <a style={inlineStyles} href="/privacy-policy">Privacy Policy</a>
        <a style={inlineStyles} href="/support">Support</a>
        <a style={inlineStyles} href="https://dfithian.github.io/2021/06/08/nomz-meal-planner.html">Blog</a>
      </div>
      <h1>Nomz</h1>
      <h2>Meal Planner & Grocery List</h2>
      <p>Nomz transforms the manual process of searching, saving, and transcribing recipes into just a few quick steps.</p>
      <div>
        <a href="https://apps.apple.com/us/app/grocer-ez/id1563273742">
          <img style={imgStyles} src={appStore} alt="Download on the App Store" />
        </a>
      </div>
      <div className="how-it-works" id="how-it-works">
        <h3>How it works</h3>
        <p>Add links to online recipes or define your own.</p>
        <div className="container" style={centerStyles}>
          <div className="item">
            <img style={imgStyles} src="/add-link.gif" alt="Add Link" width="150"/>
          </div>
          <div className="item">
            <img style={imgStyles} src="/add-recipe.gif" alt="Add Recipe" width="150"/>
          </div>
        </div>
        <p>Nomz adds the ingredients to your grocery list. You can add to, edit, merge, and reorder your list based on your local grocery store.</p>
        <div className="container" style={centerStyles}>
          <div className="item">
            <img style={imgStyles} src="/reorder.gif" alt="Reorder" width="150"/>
          </div>
          <div className="item">
            <img style={imgStyles} src="/add-manual.gif" alt="Add Groceries" width="150"/>
          </div>
        </div>
        <p>Nomz stores each recipe and shares across multiple devices, so when you're ready to cook, tap on the saved link. Add a rating and notes for the next time you make it!</p>
        <div className="container" style={centerStyles}>
          <div className="item">
            <img style={imgStyles} src="/add-notes.gif" alt="Add notes" width="150"/>
          </div>
        </div>
      </div>
    </main>
  )
}

export default IndexPage
