import * as React from "react"
import Layout from "../components/layout"
import { ContentRow } from "../components/row"

const Support = () => {
  return (
    <Layout pageTitle={"Support"}>
      <ContentRow>
        <p>
          We'd love to get your feedback.
          <br />
          Please email us at <a href="mailto:monomzapp@gmail.com">monomzapp@gmail.com</a>
        </p>
      </ContentRow>
    </Layout>
  )
}

export default Support
