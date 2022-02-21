import * as React from 'react'
import appStore from '../images/app-store.svg'
import Layout from '../components/layout.js'
import { ImgLink, H3, H4, PlainImg, Centered } from '../components/util'
import icon from '../images/icon.png'
import { ContentRow, Row } from '../components/row'

const add = process.env.PUBLIC_URL + '/screenshots/add-small.png'
const groceryList = process.env.PUBLIC_URL + '/screenshots/grocery-list-small.png'
const recipeDetail = process.env.PUBLIC_URL + '/screenshots/recipe-detail-small.png'
const recipeList = process.env.PUBLIC_URL + '/screenshots/recipe-list-small.png'

const imgStyles = {
  width: 150,
  marginLeft: 25,
  marginRight: 25,
}

const HomePage = () => {
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
          <H4>Save recipes and instantly create grocery lists. Simple, easy, and fun!</H4>
        </div>
      </Row>
      <div className="column is-6 is-offset-4">
        <Centered>
          <ImgLink href="https://apps.apple.com/us/app/grocer-ez/id1563273742" size="is-128x128" image={appStore} alt="Download on the App Store" />
        </Centered>
      </div>
      <ContentRow>
        <H4>Screenshots</H4>
      </ContentRow>
      <div className="scrolling-wrapper" id="how-it-works">
        <ContentRow>
          <div className="scroll" style={imgStyles}><ImgLink href={recipeDetail} image={recipeDetail} size="is-9by16" alt="Recipe" /></div>
          <div className="scroll" style={imgStyles}><ImgLink href={recipeList} image={recipeList} size="is-9by16" alt="Recipes" /></div>
          <div className="scroll" style={imgStyles}><ImgLink href={groceryList} image={groceryList} size="is-9by16" alt="Grocery List" /></div>
          <div className="scroll" style={imgStyles}><ImgLink href={add} image={add} size="is-9by16" alt="Add" /></div>
        </ContentRow>
      </div>
    </Layout>
  )
}

export default HomePage
