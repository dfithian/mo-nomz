import * as React from "react"
import "../styles/mystyles.scss"
import { Row } from "./row"

const Layout = ({ pageTitle, children }) => {
  const [navExpanded, setNavExpanded] = React.useState()
  return (
    <div>
      <title>{pageTitle}</title>
      <Row>
        <div className="column is-2" />
        <nav class="navbar" role="navigation" aria-label="main navigation">
          <div className="navbar-brand">
            <div className="navbar-item">
              <div className="buttons">
                <a className="button is-primary" href="/">
                  Home
                </a>
              </div>
            </div>
            <a role="button" class="navbar-burger" aria-label="menu" aria-expanded="false" onClick={() => setNavExpanded(!navExpanded)}>
              <span aria-hidden="true"></span>
              <span aria-hidden="true"></span>
              <span aria-hidden="true"></span>
            </a>
          </div>
          <div class={navExpanded ? "navbar-menu is-active" : "navbar-menu"}>
            <div class="navbar-start">
              <a class="navbar-item" href="/support">
                Support
              </a>
              <a class="navbar-item" href="/privacy-policy">
                Privacy Policy
              </a>
              <a class="navbar-item" href="https://dfithian.github.io/2021/06/08/nomz-meal-planner.html">
                Blog
              </a>
              <a class="navbar-item" href="https://venmo.com/code?user_id=1305125788319744156">
                <span role="img" aria-label="Heart" style={{color: "#183672"}}>❤</span> Donate
              </a>
            </div>
          </div>
        </nav>
      </Row>
      <main>
        {children}
      </main>
    </div>
  )
}

export default Layout
