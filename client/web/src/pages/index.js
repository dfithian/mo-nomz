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

const IndexPage = () => {
  return (
    <main className="center" style={pageStyles}>
      <title>Grocer EZ</title>
      <h1>Grocer EZ</h1>
      <p>Add recipes, get groceries.</p>
      <a href="https://apps.apple.com/us/app/grocer-ez/id1563273742">
        <img src={appStore} alt="Download on the App Store" />
      </a>
      <div>
        <a style={inlineStyles} href="/privacy-policy">Privacy Policy</a>
        <a style={inlineStyles} href="/support">Support</a>
      </div>
    </main>
  )
}

export default IndexPage
