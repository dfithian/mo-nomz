import * as React from 'react'
import Layout from '../components/layout.js'
import { H3, H5, PlainImg, Centered } from '../components/util'
import icon from '../images/icon.png'
import { Row } from '../components/row'
import { Navigate, useSearchParams } from 'react-router-dom'

const RecipePage = () => {
  const [searchParams] = useSearchParams()
  const url = searchParams.get("recipe_url")

  React.useEffect(() => {
    const timer = setTimeout(() => {
      window.location.href = url
    }, 3000)
    return () => {
      clearTimeout(timer)
    }
  })

  if (url) {
    try {
      const domain = new URL(url).hostname
      return (
        <Layout pageTitle="Nomz">
          <Row>
            <div className="column is-2 is-offset-2">
              <Centered>
                <div id="icon">
                  <PlainImg image={icon} size="is-96x96" alt="Nomz" />
                </div>
              </Centered>
            </div>
            <div className="column is-6">
              <H3>Skip the ads and life story.</H3>
            </div>
          </Row>
          <Row>
            <div className="column is-8 is-offset-2">
              <H5>You will be redirected to <b>{domain}</b> in 3 seconds</H5>
              <p>If you don't want to wait, <a href={url}>click here</a>.</p>
            </div>
          </Row>
        </Layout>
      )
    } catch {
      // will redirect to /
    }
  }
  return (<Navigate to="/" />)
}

export default RecipePage
