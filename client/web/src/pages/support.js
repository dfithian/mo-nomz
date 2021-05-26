import * as React from "react"
import { Link } from "gatsby"

const pageStyles = {
  color: "#232129",
  padding: 25,
  fontFamily: "-apple-system, Roboto, sans-serif, serif",
}

const Support = () => {
  return (
    <main className="center" style={pageStyles}>
      <h1>Grocer EZ</h1>
      <p>
        We'd love to get your feedback.
        <br />
        Please email us at <a href="mailto:monomzapp@gmail.com">monomzapp@gmail.com</a>
      </p>
    </main>
  )
}

export default Support