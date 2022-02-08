import * as React from "react"
import appStore from "../images/app-store.svg"
import Layout from "../components/layout.js"
import { ImgLink, H3, H4, PlainImg, Centered } from "../components/util"
import icon from "../images/icon.png"
import { ContentRow, Row } from "../components/row"

const add = "/screenshots/add-small.png"
const groceryList = "/screenshots/grocery-list-small.png"
const recipeDetail = "/screenshots/recipe-detail-small.png"
const recipeList = "/screenshots/recipe-list-small.png"

const imgStyles = {
  width: 150,
  marginLeft: 25,
  marginRight: 25,
}

const IndexPage = () => {
  return (
    <Layout pageTitle="Nomz">
      <Row>
        <div className="column is-4 is-offset-2">
          <Centered>
            <div id="icon">
              <PlainImg image={icon} size="is-128x128" alt="Nomz" />
            </div>
          </Centered>
        </div>
        <div className="column is-4">
          <H3 text="Skip the ads and life story." />
          <H4 text="Save recipes and instantly create grocery lists. Simple, easy, and fun!" />
        </div>
      </Row>
      <div className="column is-4 is-offset-6">
        <Centered>
          <ImgLink href="https://apps.apple.com/us/app/grocer-ez/id1563273742" size="is-128x128" image={appStore} alt="Download on the App Store" />
        </Centered>
      </div>
      <ContentRow>
        <H4 text="Screenshots" />
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

export default IndexPage
