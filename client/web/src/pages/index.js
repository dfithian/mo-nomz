import * as React from "react"
import appStore from "../images/app-store.svg"
import Layout from "../components/layout.js"
import { Img, ImgLink, H2, H3, PlainImg, Msg } from "../components/util"
import icon from "../images/icon.png"
import addLink from "../images/add-link.gif"
import addManual from "../images/add-manual.gif"
import addRecipe from "../images/add-recipe.gif"
import addNotes from "../images/add-notes.gif"
import reorder from "../images/reorder.gif"
import { Col, Col4, ContentCol, ContentRow, Row } from "../components/row"

const imgStyles = {
  width: 150,
}

const IndexPage = () => {
  return (
    <Layout pageTitle={"Nomz"}>
      <Row>
        <Col />
        <Col>
          <Img image={icon} size={"is-128x128"} alt={"Nomz"} />
        </Col>
        <ContentCol>
          <H2 text={"Skip the ads and life story."} />
          <H3 text={"Save recipes and instantly create a grocery list."} />
        </ContentCol>
      </Row>
      <Row>
        <Col />
        <Col>
          <ImgLink href={"https://apps.apple.com/us/app/grocer-ez/id1563273742"} size={"is-128x128"} image={appStore} alt={"Download on the App Store"} />
        </Col>
      </Row>
      {/** FIXME screenshots */}
      <div className="how-it-works" id="how-it-works">
        <ContentRow>
          <H3 text={"How it works"} />
        </ContentRow>
        <Msg>
          <p>Nomz transforms the manual process of searching, saving, and transcribing recipes into a few quick steps.</p>
        </Msg>
        <Msg>
          <Row>
            <Col4>
              <p>Add links to online recipes or define your own</p>
            </Col4>
            <Col4><div style={imgStyles}><PlainImg image={addLink} size={"is-9by16"} alt={"Add Link"} /></div></Col4>
            <Col4><div style={imgStyles}><PlainImg image={addRecipe} size={"is-9by16"} alt={"Add Recipe"} /></div></Col4>
          </Row>
        </Msg>
        <Msg>
          <Row>
            <Col4>
              <p>Nomz adds the ingredients to your grocery list. Youc an add to, edit, merge, and reorder your list based on your local grocery store.</p>
            </Col4>
            <Col4><div style={imgStyles}><PlainImg image={reorder} size={"is-9by16"} alt={"Reorder"} /></div></Col4>
            <Col4><div style={imgStyles}><PlainImg image={addManual} size={"is-9by16"} alt={"Add Manual"} /></div></Col4>
          </Row>
        </Msg>
        <Msg>
          <Row>
            <Col4>
              <p>Nomz stores each recipe and shares across multiple devices. When you're ready to cook, just tap on the item. Add a rating and notes for the next time you make it!</p>
            </Col4>
            <Col4><div style={imgStyles}><PlainImg image={addNotes} size={"is-9by16"} alt={"Add Notes"} /></div></Col4>
          </Row>
        </Msg>
      </div>
    </Layout>
  )
}

export default IndexPage
