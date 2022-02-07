import * as React from "react"

const Row = ({ children }) => {
  return (
    <div className="columns is-mobile is-vcentered">
      {children}
    </div>
  )
}

const Col = ({ children }) => {
  return (
    <div className="column is-2">
      {children}
    </div>
  )
}

const ContentCol = ({ children }) => {
  return (
    <div className="column is-8">
      {children}
    </div>
  )
}

const ContentRow = ({ children }) => {
  return (
    <Row>
      <Col />
      <ContentCol>
        {children}
      </ContentCol>
    </Row>
  )
}

const Col4 = ({ children }) => {
  return (
    <div className="column is-4">
      {children}
    </div>
  )
}

export {
  Row,
  Col,
  ContentCol,
  ContentRow,
  Col4,
}
