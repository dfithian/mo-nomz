import * as React from "react"
import "../styles/mystyles.scss"
import { Col, Row } from "./row"

const Layout = ({ pageTitle, children }) => {
  return (
    <div>
      <title>{pageTitle}</title>
      <Row>
        <Col />
        <nav class="navbar" role="navigation" aria-label="main navigation">
          <div class="navbar-menu is-active">
            <div class="navbar-start">
              <div className="navbar-item">
                <div className="buttons">
                  <a className="button is-primary" href="/">
                    Home
                  </a>
                </div>
              </div>
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
