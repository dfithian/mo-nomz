import * as React from "react"
import appStore from "../images/app-store.svg"
import Layout from "../components/layout.js"
import { ImgLink, H2, H3, PlainImg, Centered } from "../components/util"
import icon from "../images/icon.png"
import { Col, ContentCol, ContentRow, Row } from "../components/row"

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
    <Layout pageTitle={"Nomz"}>
      <Row>
        <Col />
        <Col>
          <Centered>
            <PlainImg image={icon} size={"is-128x128"} alt={"Nomz"} />
          </Centered>
        </Col>
        <ContentCol>
          <H2 text={"Skip the ads and life story."} />
          <H3 text={"Save recipes and instantly create grocery lists."} />
        </ContentCol>
      </Row>
      <Row>
        <Col />
        <Col>
          <Centered>
            <ImgLink href={"https://apps.apple.com/us/app/grocer-ez/id1563273742"} size={"is-128x128"} image={appStore} alt={"Download on the App Store"} />
          </Centered>
        </Col>
      </Row>
      <div className="scrolling-wrapper" id="how-it-works">
        <ContentRow>
          <H3 text={"Screenshots"} />
        </ContentRow>
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
