import * as React from 'react'
import { Link } from 'react-router-dom'
import Layout from '../components/layout'
import { Row } from '../components/row'

const NotFoundPage = () => {
  return (
    <Layout pageTitle="Not Found">
      <Row>
        <div className="column is-8 is-offset-2">
          <p>
            Sorry, we couldn't find what you were looking for.
            <br />
            <Link to="/">Go home</Link>.
          </p>
        </div>
      </Row>
    </Layout>
  )
}

export default NotFoundPage
