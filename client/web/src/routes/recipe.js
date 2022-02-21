import * as React from 'react'
import appStore from '../images/app-store.svg'
import Layout from '../components/layout.js'
import { H3, H4, H5, ImgLink, PlainImg, Centered } from '../components/util'
import icon from '../images/icon.png'
import { Row } from '../components/row'
import { Navigate, useSearchParams } from 'react-router-dom'

const RecipePage = () => {
  const [searchParams] = useSearchParams()
  const url = searchParams.get("recipe_url")

  React.useEffect(() => {
    const timer = setTimeout(() => {
      window.location.href = url
    }, 5000)
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
            <div className="column is-3 is-offset-2">
              <Centered>
                <div id="icon">
                  <PlainImg image={icon} size="is-96x96" alt="Nomz" />
                </div>
              </Centered>
            </div>
            <div className="column is-5">
              <H3>Skip the life story.</H3>
              <H4>Download Nomz to easily store, share, and shop for recipes!</H4>
            </div>
          </Row>
          <Row>
            <div className="column is-5 is-offset-3">
              <Centered>
                <ImgLink href="https://apps.apple.com/us/app/grocer-ez/id1563273742" size="is-128x128" image={appStore} alt="Download on the App Store" />
              </Centered>
            </div>
          </Row>
          <Row>
            <div className="column is-8 is-offset-2">
              <H5>You will be redirected to <b>{domain}</b> in 5 seconds</H5>
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
